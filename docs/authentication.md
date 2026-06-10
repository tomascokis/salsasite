# Authentication Contracts

These contracts cover login, access control, and role behavior.

## Access Model

- All application content requires an authenticated user.
- Unauthenticated users may access only the login workflow and required framework assets.
- Move pages, overview pages, progress pages, dancer pages, media browsing, media files, poster files, and read APIs must not be publicly accessible.
- Authenticated users have exactly one role: `viewer` or `admin`.
- Viewers may browse and read gated content.
- Admins may browse, create, edit, upload, render, publish, retry, repair, undo, and manage operational settings.

## Admin-Only Areas

- Move creation and editing workflows are admin-only.
- Source upload, source metadata editing, clip editing, rendering, publishing, key-video changes, and deletion are admin-only.
- Progress editing is admin-only.
- Operational settings, history undo, media diagnostics, media repair, and media job retry workflows are admin-only.
- UI controls for admin-only workflows should be hidden from viewers, but server-side authorization is the source of truth.

## Sessions And Accounts

- Accounts are local to the deployment and stored in the app-state SQLite database.
- If no users exist yet, the app creates a default admin account for `tomascokis` on the first login attempt.
- Additional account creation and operator-driven password resets are performed by a local CLI seed script, not by public signup.
- Account seed/reset scripts must refuse to write users when the resolved data directory does not match the running deployment's live `DATA_DIR`.
- Authenticated users may change their own password from an account panel after confirming their current password.
- Login sessions use an HTTP-only cookie with a long private-device expiry.
- Logout must invalidate the server-side session and clear the browser cookie.
- Passwords must never be stored directly; only salted password hashes may be persisted.
- Authentication diagnostics may write non-secret password metadata, such as length and verification outcome, to `DATA_DIR/auth-debug.log`; they must never write plaintext passwords or reusable password hashes.

## Security Tracking

- The app records authenticated page views and video requests per user in hourly SQLite rollups.
- The app records unique IP addresses used by each account per UTC week.
- The app records login page views by IP and login attempts by attempted username, IP, and success/failure result.
- Security tracking is server-side and must not depend on browser-side scripts.
- Security tracking is warning-only except for IP auto-bans on login abuse; auto-bans must not lock usernames or deactivate accounts.
- Login abuse auto-bans apply by IP address after 20 failed login submissions in one hour or 120 login page views in one hour, expire after 24 hours, and can be manually lifted by an admin.
- The admin-only `/settings/security` page shows usage summaries, login activity, recent attempts, and warning states.
- Raw IP tracking data is retained indefinitely.
- Proxy headers are trusted only when `TRUST_PROXY_HEADERS=true`; otherwise the app uses the direct client address reported by SvelteKit.

## Audit Actors

- New mutating actions should record the authenticated actor in action history.
- Existing history rows without an actor remain valid and should be treated as unknown or system actions.
