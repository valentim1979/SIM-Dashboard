#!/bin/bash
# =============================================================================
# PUBLICAR SITE SIM — 15ª RS NO GITHUB PAGES
# Uso: bash publicar.sh "mensagem do commit"
# =============================================================================

set -e

MENSAGEM=${1:-"Atualização dos indicadores SIM"}
SITE_DIR="/Users/valentimsalajunior/Desktop/R_SIM/sim_site"
OUTPUTS="/Users/valentimsalajunior/Desktop/R_SIM/SIM/outputs"
GRAFICOS="$SITE_DIR/graficos"

echo ""
echo "════════════════════════════════════════"
echo " PUBLICAÇÃO SIM — 15ª RS"
echo "════════════════════════════════════════"

# 1. Copiar PNGs para pasta graficos/ dentro do site
echo ""
echo "▶ Copiando gráficos para o site..."
mkdir -p "$GRAFICOS"
N_PNG=$(ls "$OUTPUTS"/*.png 2>/dev/null | wc -l | tr -d ' ')
if [ "$N_PNG" -eq 0 ]; then
  echo "✗ Nenhum PNG encontrado em: $OUTPUTS"
  echo "  Execute primeiro os scripts R de geração de gráficos."
  exit 1
fi
cp "$OUTPUTS"/*.png "$GRAFICOS/"
echo "  ✓ $N_PNG arquivo(s) copiado(s) para graficos/"

# 2. Renderizar o site
echo ""
echo "▶ Renderizando site Quarto..."
cd "$SITE_DIR"
quarto render
echo "  ✓ Site renderizado em docs/"

# 3. Commit e push
echo ""
echo "▶ Enviando para GitHub..."
git add .
git commit -m "$MENSAGEM — $(date '+%d/%m/%Y %H:%M')" || echo "  (nada novo para commitar)"
git push origin main

echo ""
echo "════════════════════════════════════════"
echo " Concluído!"
echo " Site: https://valentim1979.github.io/SIM-Dashboard"
echo "════════════════════════════════════════"
echo ""
