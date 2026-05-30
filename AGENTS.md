# Agent Instructions

## Environment

You are on a dev machine, which is directly accessing the live project folders on the deployment machine. For browser verification of the running site, use `http://192.168.0.127:18096` instead of starting or testing against a localhost dev server unless the user explicitly asks for local-only testing.

Don't run `npm run check` on the local machine.  

## Documentation Contracts

When the user gives an explicit instruction for how a page, workflow, feature, or topic should work, capture it as a durable contract in the relevant documentation for that area.

Before making changes to an area, review the existing contracts that apply to that page, workflow, feature, or topic and preserve them.

If a new user instruction appears to conflict with, replace, weaken, or remove an existing contract, ask the user to confirm the intended change before implementing it. Do not silently discard prior contracts.

## Development

When a feature is fully completed, commit to git. Do not add videos to git.

## Sizing

When the user asks for something to trigger a web element overlaps, doesn't fit, or goes past a size, do not guess thresholds if they cannot be fully determined. Instead, use a method that detects the change needed, or test set in browser to verify the precise threshold.