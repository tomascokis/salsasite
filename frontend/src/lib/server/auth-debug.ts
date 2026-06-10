import fs from 'node:fs';
import path from 'node:path';
import { resolveDataDir } from './paths';

type AuthDebugEvent = {
  event: 'password_set' | 'login_attempt';
  username: string;
  usernameNormalized: string;
  role?: string;
  existingUser?: boolean;
  userFound?: boolean;
  isActive?: boolean;
  passwordVerified?: boolean;
  reason?: string;
} & PasswordDebugInfo;

export type PasswordDebugInfo = {
  passwordLength: number;
  passwordByteLength: number;
  hasLeadingOrTrailingWhitespace: boolean;
  containsNewline: boolean;
};

export function passwordDebugInfo(password: string): PasswordDebugInfo {
  return {
    passwordLength: password.length,
    passwordByteLength: Buffer.byteLength(password, 'utf8'),
    hasLeadingOrTrailingWhitespace: password !== password.trim(),
    containsNewline: /[\r\n]/.test(password)
  };
}

export function appendAuthDebugLog(event: AuthDebugEvent) {
  const entry = {
    timestamp: new Date().toISOString(),
    ...event
  };
  try {
    const logPath = path.join(resolveDataDir(), 'auth-debug.log');
    fs.mkdirSync(path.dirname(logPath), { recursive: true });
    fs.appendFileSync(logPath, `${JSON.stringify(entry)}\n`, 'utf8');
  } catch (error) {
    console.warn('Failed to write auth debug log', error);
  }
}
