# SIM — 15ª RS Maringá | GitHub Pages

Site de indicadores de mortalidade da 15ª Regional de Saúde de Maringá/PR,
produzido com Quarto e publicado via GitHub Pages.

**URL:** https://valentim1979.github.io/sim-15rs

---

## Estrutura

```
sim_site/
├── _quarto.yml          # configuração do site
├── index.qmd            # página inicial
├── demografico.qmd      # perfil demográfico
├── causas.qmd           # causas de morte
├── assistencia.qmd      # assistência e local
├── municipios.qmd       # municípios e taxas
├── suicidio.qmd         # mortalidade por suicídio
├── qualidade.qmd        # qualidade da informação
├── assets/
│   └── custom.css       # paleta 15ª RS
├── publicar.sh          # script de publicação
└── docs/                # site renderizado (gerado pelo Quarto)
```

## Workflow de atualização

```bash
# 1. Gerar os dados e gráficos no R
Rscript indicadores_sim_15rs.R
Rscript grafico_suicidio_15rs.R

# 2. Publicar
cd sim_site
bash publicar.sh "Atualização jan/2025"
```

## Configuração inicial do GitHub Pages

1. Crie o repositório `sim-15rs` em github.com/valentim1979
2. Configure GitHub Pages: **Settings → Pages → Source: Deploy from branch → main → /docs**
3. Execute `publicar.sh` pela primeira vez

---

**Fonte dos dados:** SIM/DATASUS  
**Elaboração:** SCVGE — 15ª RS Maringá/PR
# SIM-Dashboard
