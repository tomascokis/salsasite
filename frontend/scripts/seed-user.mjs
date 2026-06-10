import { createHash, randomBytes, randomUUID, scrypt as scryptCallback } from 'node:crypto';
import { DatabaseSync } from 'node:sqlite';
import fs from 'node:fs';
import path from 'node:path';
import readline from 'node:readline/promises';
import { stdin as input, stdout as output } from 'node:process';
import { promisify } from 'node:util';

const scrypt = promisify(scryptCallback);
const SCRYPT_KEY_LENGTH = 64;
const SCRYPT_OPTIONS = { N: 16384, r: 8, p: 1 };

function nowIso() {
  return new Date().toISOString();
}

function parseArgs(argv) {
  const result = {
    username: '',
    role: 'viewer',
    update: false,
    passwordStdin: false
  };

  for (let index = 0; index < argv.length; index += 1) {
    const arg = argv[index];
    if (arg === '--username') {
      result.username = argv[++index] ?? '';
    } else if (arg === '--role') {
      result.role = argv[++index] ?? '';
    } else if (arg === '--update') {
      result.update = true;
    } else if (arg === '--password-stdin') {
      result.passwordStdin = true;
    } else {
      throw new Error(`Unknown argument: ${arg}`);
    }
  }

  return result;
}

function resolveDataDir() {
  return process.env.DATA_DIR || path.resolve(process.cwd(), '..', 'data', 'live');
}

function passwordDebugInfo(password) {
  return {
    passwordLength: password.length,
    passwordByteLength: Buffer.byteLength(password, 'utf8'),
    hasLeadingOrTrailingWhitespace: password !== password.trim(),
    containsNewline: /[\r\n]/.test(password)
  };
}

function appendAuthDebugLog(dataDir, event) {
  const entry = {
    timestamp: nowIso(),
    ...event
  };
  try {
    fs.mkdirSync(dataDir, { recursive: true });
    fs.appendFileSync(path.join(dataDir, 'auth-debug.log'), `${JSON.stringify(entry)}\n`, 'utf8');
  } catch (error) {
    console.warn('Failed to write auth debug log', error);
  }
}

function normalizeUsername(username) {
  return String(username ?? '').replace(/\s+/g, ' ').trim().toLocaleLowerCase();
}

function validateRole(role) {
  if (role === 'viewer' || role === 'admin') {
    return role;
  }
  throw new Error('Role must be viewer or admin.');
}

async function readPassword(passwordStdin) {
  if (passwordStdin) {
    return await new Promise((resolve, reject) => {
      let data = '';
      process.stdin.setEncoding('utf8');
      process.stdin.on('data', (chunk) => {
        data += chunk;
      });
      process.stdin.on('end', () => resolve(data.replace(/\r?\n$/, '')));
      process.stdin.on('error', reject);
    });
  }

  if (!process.stdin.isTTY) {
    throw new Error('Use --password-stdin when piping a password.');
  }

  const rl = readline.createInterface({ input, output });
  try {
    const password = await rl.question('Password: ');
    const confirm = await rl.question('Confirm password: ');
    if (password !== confirm) {
      throw new Error('Passwords do not match.');
    }
    return password;
  } finally {
    rl.close();
  }
}

async function hashPassword(password) {
  const salt = randomBytes(16).toString('base64url');
  const derivedKey = await scrypt(password, salt, SCRYPT_KEY_LENGTH, SCRYPT_OPTIONS);
  return [
    'scrypt',
    String(SCRYPT_OPTIONS.N),
    String(SCRYPT_OPTIONS.r),
    String(SCRYPT_OPTIONS.p),
    salt,
    Buffer.from(derivedKey).toString('base64url')
  ].join('$');
}

function createSchema(db) {
  db.exec(`
    CREATE TABLE IF NOT EXISTS auth_users (
      id TEXT PRIMARY KEY,
      username TEXT NOT NULL,
      username_normalized TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      role TEXT NOT NULL,
      is_active INTEGER NOT NULL,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      last_login_at TEXT
    );

    CREATE INDEX IF NOT EXISTS idx_auth_users_role ON auth_users(role);

    CREATE TABLE IF NOT EXISTS auth_sessions (
      id TEXT PRIMARY KEY,
      user_id TEXT NOT NULL,
      token_hash TEXT NOT NULL UNIQUE,
      created_at TEXT NOT NULL,
      expires_at TEXT NOT NULL,
      last_seen_at TEXT NOT NULL,
      FOREIGN KEY(user_id) REFERENCES auth_users(id) ON DELETE CASCADE
    );

    CREATE INDEX IF NOT EXISTS idx_auth_sessions_token_hash ON auth_sessions(token_hash);
    CREATE INDEX IF NOT EXISTS idx_auth_sessions_user ON auth_sessions(user_id);
    CREATE INDEX IF NOT EXISTS idx_auth_sessions_expires_at ON auth_sessions(expires_at);
  `);
}

async function main() {
  const args = parseArgs(process.argv.slice(2));
  const username = String(args.username ?? '').replace(/\s+/g, ' ').trim();
  const usernameNormalized = normalizeUsername(username);
  const role = validateRole(args.role);
  if (!usernameNormalized) {
    throw new Error('Pass --username USER.');
  }

  const password = await readPassword(args.passwordStdin);
  if (password.length < 8) {
    throw new Error('Password must be at least 8 characters.');
  }

  const dataDir = resolveDataDir();
  fs.mkdirSync(dataDir, { recursive: true });
  const db = new DatabaseSync(path.join(dataDir, 'app-state.sqlite'));
  db.exec('PRAGMA foreign_keys = ON');
  createSchema(db);

  const existing = db
    .prepare('SELECT id, created_at, last_login_at FROM auth_users WHERE username_normalized = ? LIMIT 1')
    .get(usernameNormalized);
  if (existing && !args.update) {
    throw new Error('User already exists. Re-run with npm run user:password or --update to change it.');
  }

  const timestamp = nowIso();
  const id = existing?.id ?? randomUUID();
  const passwordHash = await hashPassword(password);
  db.prepare(
    `
      INSERT INTO auth_users (
        id, username, username_normalized, password_hash, role, is_active,
        created_at, updated_at, last_login_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT(username_normalized) DO UPDATE SET
        username = excluded.username,
        password_hash = excluded.password_hash,
        role = excluded.role,
        is_active = excluded.is_active,
        updated_at = excluded.updated_at
    `
  ).run(
    id,
    username,
    usernameNormalized,
    passwordHash,
    role,
    1,
    existing?.created_at ?? timestamp,
    timestamp,
    existing?.last_login_at ?? null
  );

  appendAuthDebugLog(dataDir, {
    event: 'password_set',
    username,
    usernameNormalized,
    role,
    existingUser: Boolean(existing),
    ...passwordDebugInfo(password)
  });

  const tokenPreview = createHash('sha256').update(id).digest('hex').slice(0, 8);
  console.log(`${existing ? 'Updated' : 'Created'} ${role} user "${username}" (${tokenPreview}).`);
  db.close();
}

main().catch((error) => {
  console.error(error instanceof Error ? error.message : error);
  process.exitCode = 1;
});
