#!/bin/bash
# =============================================================================
# PUBLICAR SITE SIM — 15ª RS NO GITHUB PAGES
# =============================================================================
# Pré-requisitos:
#   - Quarto instalado (https://quarto.org)
#   - Git configurado com acesso ao repositório
#   - Scripts R já executados (PNGs gerados em R_SIM/SIM/outputs/)
#
# Uso:
#   cd /Users/valentimsalajunior/Desktop/R_SIM/sim_site
#   bash publicar.sh "Atualização indicadores 2025"
# =============================================================================

set -e  # interrompe se qualquer comando falhar

MENSAGEM=${1:-"Atualização dos indicadores SIM"}
SITE_DIR="/Users/valentimsalajunior/Desktop/R_SIM/sim_site"
OUTPUTS="/Users/valentimsalajunior/Desktop/R_SIM/SIM/outputs"

echo ""
echo "════════════════════════════════════════"
echo " PUBLICAÇÃO SIM — 15ª RS"
echo "════════════════════════════════════════"

# 1. Verificar se há PNGs gerados
echo ""
echo "▶ Verificando arquivos de imagem..."
N_PNG=$(ls "$OUTPUTS"/*.png 2>/dev/null | wc -l | tr -d ' ')
if [ "$N_PNG" -eq 0 ]; then
  echo "✗ Nenhum PNG encontrado em: $OUTPUTS"
  echo "  Execute primeiro: source('indicadores_sim_15rs.R')"
  exit 1
fi
echo "  ✓ $N_PNG arquivo(s) PNG encontrado(s)"

# 2. Renderizar o site com Quarto
echo ""
echo "▶ Renderizando site Quarto..."
cd "$SITE_DIR"
quarto render
echo "  ✓ Site renderizado em: $SITE_DIR/docs/"

# 3. Verificar se é repositório git
if [ ! -d ".git" ]; then
  echo ""
  echo "▶ Inicializando repositório Git..."
  git init
  git remote add origin https://github.com/valentim1979/sim-15rs.git
  echo "  ✓ Repositório inicializado"
  echo "  ⚠ Crie o repositório 'sim-15rs' em github.com/valentim1979 antes do push"
fi

# 4. Commit e push
echo ""
echo "▶ Enviando para GitHub..."
git add .
git commit -m "$MENSAGEM — $(date '+%d/%m/%Y %H:%M')" || echo "  (nada novo para commitar)"
git push origin main 2>/dev/null || git push origin master 2>/dev/null || {
  echo "  ⚠ Push falhou. Verifique:"
  echo "    1. O repositório existe em github.com/valentim1979/sim-15rs"
  echo "    2. Você está autenticado no GitHub"
  echo "    3. A branch está correta (main ou master)"
}

echo ""
echo "════════════════════════════════════════"
echo " Concluído!"
echo " Site: https://valentim1979.github.io/sim-15rs"
echo "════════════════════════════════════════"
echo ""
