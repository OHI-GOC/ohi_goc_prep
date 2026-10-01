# ICO v2026 — expert-final iconic list (2026-09-30)

This folder is a copy of `ICO/v2026`. I updated it to use only the species the experts marked Keep (43 kept and 36 removed).

On mazu, the Keep-only working list is `/home/shares/ohi/OHI_GOC/goal_prep/sp/ico/especies_iconicas_093026.csv`, and that is what `script1` reads. The full Keep/Remove expert sheet is archived there as `sp_ico_list_w_general_groups_093026.csv`. I updated `script1_iconic_species_list.qmd` to read the Keep list and write dated intermediates (`*_093026`). Scripts 2 through 5 still use the same pipeline logic, so I will re-run them after script1 so the condition joins use the new species–region table.

The criteria from the OHI Core Team email to the experts in September 2026 are that iconic species must have local cultural meaning beyond seafood, be Gulf-dependent (marine or coastal obligate), and matter to the general coastal community. Existence value alone is not sufficient.
