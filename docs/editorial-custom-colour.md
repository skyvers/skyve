# Editorial custom colour schemes

The Editorial template supports client branding with thin CSS files that override a few `--skyve-editorial-*` variables. The rest of the layout (divider, focus ring, menu highlights, buttons, table actions, and the view title bar) derives from those values automatically.

## Built-in colours

Set the theme colour on the UxUi in `skyve-war/src/main/java/router/UxUis.java`:

```java
private static final String THEME_COLOUR = "custom";
```

Available values: `blue` (default), `indigo`, `emerald`, `custom`, and `dark`.

- `blue`, `indigo`, `emerald`, and `custom` use their named palette in light mode and
  follow the browser/OS preference into the dark palette.
- `dark` forces the dark palette and PrimeFaces Arya theme regardless of browser/OS preference.
- `custom` is a starter corporate palette (`#004B88`) intended to be copied and edited per project.

Example:

```java
UxUi.newPrimeFaces("phone", "editorial", "skyve", "custom");
```

## How a colour is wired

The exact PrimeFaces base theme and its colour palette are configured separately. In the
example above, `skyve` selects the base theme and `custom` selects the Editorial palette and
the corresponding PrimeFaces widget colour overrides.

1. **Shared PrimeFaces theme** — `primefaces-skyve/theme.css` imports free saga when the browser
   prefers light and free arya when it prefers dark. Selecting the `dark` colour makes
   `UxUis.java` use `primefaces-skyve-dark/theme.css`, which always imports arya.
2. **Layout palette** — the selected colour file overrides `--skyve-editorial-*` CSS variables
   after `main-min.css`.
3. **Dark layout palette** — `dark.css` loads after the selected palette under the same
   `prefers-color-scheme: dark` media query as arya.

Every loaded palette URL carries Skyve's web-resource cache version.

| Kind | Location |
|---|---|
| PrimeFaces aliases | `skyve-web/src/main/resources/META-INF/resources/primefaces-skyve*/theme.css` |
| Built-in layout palettes | `skyve-war/.../editorial/assets/css/themes/<colour>.css` |
| Project branding palettes | same layout palettes folder in the WAR |

## Create a new colour scheme

1. **Copy the starter palette**:

   ```bash
   cp skyve-war/src/main/webapp/WEB-INF/resources/skyve/editorial/assets/css/themes/custom.css \
      skyve-war/src/main/webapp/WEB-INF/resources/skyve/editorial/assets/css/themes/mybrand.css
   ```

2. **Edit the brand knobs** in `mybrand.css`:

   ```css
   :root {
       /* Primary brand colour as R, G, B (used for rgba() derived tokens) */
       --skyve-editorial-accent-rgb: 0, 75, 136;
       /* Darker hover / pressed accent */
       --skyve-editorial-accent-hover: #00447A;
       /* Lighter equivalents that retain contrast on dark surfaces */
       --skyve-editorial-dark-accent-rgb: 92, 164, 214;
       --skyve-editorial-dark-accent-hover: #3D8FC5;
       /* Optional: override the view title bar (defaults to the accent) */
       /* --skyve-editorial-topbar-bg: #003C6D; */
   }
   ```

   | Variable | Purpose |
   |---|---|
   | `--skyve-editorial-accent-rgb` | Primary brand colour as `R, G, B`. Also drives `--skyve-editorial-accent` and soft tints (focus ring, menu highlights, buttons). |
   | `--skyve-editorial-accent-hover` | Darker accent for hover / pressed states. |
   | `--skyve-editorial-dark-accent-rgb` | Lighter version of the brand colour used for highlights when the browser prefers dark mode. |
   | `--skyve-editorial-dark-accent-hover` | Dark-mode hover / pressed accent. |
   | `--skyve-editorial-topbar-bg` | View title bar background. Omit to use the accent colour. |

   The dark palette consumes the two dark-accent variables while retaining its shared dark
   surfaces, borders, and text. If either variable is omitted, the existing dark blue is used
   as its fallback.

3. **Select it** in `UxUis.java`:

   ```java
   private static final String THEME_COLOUR = "mybrand";
   ```

4. Redeploy the WAR (or republish in the IDE) and hard-refresh the browser.

### Or edit `custom` in place

For a single-client project it is often enough to edit:

- `skyve-war/.../themes/custom.css`

The palette is intentionally readable and does not require a minification step. Leave `THEME_COLOUR = "custom"`.

## Optional overrides

Most branding only needs the knobs above. If a surface still looks wrong, override additional tokens from `main.css` in the same `:root` block, for example:

```css
:root {
	--skyve-editorial-accent-rgb: 0, 75, 136;
	--skyve-editorial-accent-hover: #00447A;
	--skyve-editorial-dark-accent-rgb: 92, 164, 214;
	--skyve-editorial-dark-accent-hover: #3D8FC5;
	--skyve-editorial-page-bg: #f5f7fa;
	--skyve-editorial-sidebar-bg: #ffffff;
}
```

Inspect `:root` at the top of `skyve-web/src/css/editorial/main.css` for the full token list.

## Limits

Editorial colour schemes recolour layout chrome and the PrimeFaces controls that Editorial restyles through CSS variables (buttons, panels, menus, and similar). Some free saga and arya widget rules still hard-code their own colours; override those selectively in CSS if needed.

## Dark logos

If a customer resource named `<logoName>-dark.<extension>` exists beside the configured logo (for example `mylogo-dark.png` for `mylogo.png`), it is used automatically when the browser prefers dark mode or `dark` is selected permanently.
