#!/usr/bin/env bash
# A 4 zart ("lakat"-jelzesu) oldalt titkositja staticrypttel, quarto render
# utan, mielott docs/ git-be kerulne. A repo publikus, ezert a jelszo SOHA
# nem kerulhet commitolt fajlba -- a STATICRYPT_PASSWORD env valtozobol jon
# (allitsd be pl. a ~/.zshrc-ben, ne a repoban).
set -euo pipefail
cd "$(dirname "$0")/.."

if [ -z "${STATICRYPT_PASSWORD:-}" ]; then
  echo "HIBA: a STATICRYPT_PASSWORD env valtozo nincs beallitva." >&2
  echo "A zart oldalak titkositas NELKUL maradnanak a publikus docs/-ban -- leallitva." >&2
  echo "Allitsd be: export STATICRYPT_PASSWORD='...' es probald ujra." >&2
  exit 1
fi

LOCKED_FILES=(
  "docs/kosarlabda/analytics.html"
  "docs/futball/analytics.html"
  "docs/futball/team_analytics.html"
  "docs/futball/nb1_elorejelzesek.html"
)

for f in "${LOCKED_FILES[@]}"; do
  if [ ! -f "$f" ]; then
    echo "Kihagyva (nem talalhato): $f" >&2
    continue
  fi
  if grep -q 'staticrypt-html' "$f"; then
    echo "Mar titkositva, kihagyva: $f"
    continue
  fi
  dir=$(dirname "$f")
  echo "Titkositas: $f"
  npx staticrypt "$f" -d "$dir" -c .staticrypt.json --short \
    --template-title "Vedett oldal" \
    --template-instructions "Ez az oldal jelszoval vedett tartalom." \
    --template-placeholder "Jelszo" \
    --template-button "Belepes" \
    --template-error "Hibas jelszo!"
done

echo "Kesz: zart oldalak titkositasa lefutott."
