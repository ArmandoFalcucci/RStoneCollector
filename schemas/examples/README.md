# Example schemas

Drop fully worked `.cfg` files here to study or copy. They show up in the app's
schema picker (Settings → Active schema) with an `[example]` prefix, so they
never get in the way of your own working schema.

Suggested contents:

- `boomplaas.cfg` — the merged Boomplaas Cave flake + core recording schema.
  If you have your working copy, place it here and it becomes selectable in the
  app as `[example] boomplaas.cfg`.

To use an example as your real schema, either select it in the welcome dialog /
Settings, or copy it up one level and edit it:

```bash
cp schemas/examples/boomplaas.cfg schemas/my-site.cfg
```

Then point the project at `schemas/my-site.cfg` in Settings.
