# AI Skills

Reusable skill definitions for AI coding assistants working with the Daedalus ecosystem.

## Usage

To use a skill, copy its folder into your project's `.claude/skills/` directory:

```bash
cp -r docs/ai-skills/daedalus-ddl /path/to/your-project/.claude/skills/
```

The assistant will automatically activate the skill when it encounters relevant tasks (e.g., editing `.ddl` files).

## Available Skills

| Skill | Description |
|-------|-------------|
| `daedalus-ddl` | Writing, reading, and debugging Daedalus (`.ddl`) format specifications and Dex (`.dex`) export specs |
