# data-mining-project
Repo for data mining group project

## Folder structure

| Folder     | What goes here                                  |
|------------|-------------------------------------------------|
| `scripts/` | R scripts                                       |
| `outputs/` | Outputs produced by the scripts (plots, tables) |
| `data/`    | Datasets used by the scripts                    |

> GitHub rejects files over 100 MB. If a dataset is bigger than that, share it another way and don't commit it.

## Data

`data/ess_baltic_R9-R11.csv` — adapted from the European Social Survey (ESS):
ESS Round 9 (edition 3.3), Round 10 (edition 3.3), Round 10 self-completion (edition 3.2)
and Round 11 (edition 4.2); countries EE, LT, LV; selected variables only
(original file name: `ESS9e03_3-ESS10e03_3-ESS10SCe03_2-ESS11e04_2-subset.csv`).

- 12,791 respondents (LT 4,859, LV 3,193, EE 4,739).
- Source: [ESS Data Portal](https://ess.sikt.no/). Licence: [CC BY-NC-SA 4.0](https://europeansocialsurvey.org/node/58)
  (non-commercial use, cite the source).
- ⚠️ **Keep this repository private.** ESS asks not to publish its datasets on external websites.
- Latvia Round 10 comes from the self-completion file (web/paper, not face-to-face) — answers may differ because of the survey mode.

### How to use the data (everyone the same way)

1. Run `scripts/01_clean.R` once. It turns ESS missing codes (7/8/9, 77/88/99, 666–999) into `NA`,
   makes age groups and simple yes/no variables, and saves `data/ess_baltic_clean.rds`
   (this file is not uploaded — everyone makes it on their own computer).
2. Start your own script with `source("scripts/survey_design.R")`. You get:
   - `d` = the clean data,
   - `des` = the data with survey weights (`anweight`) for the `survey` package.
3. Packages needed (install once): `install.packages(c("dplyr", "survey"))`
4. Working directory in R must be the repository folder (`data-mining-project`).

## How to contribute

### 1. One-time setup

Install [Git](https://git-scm.com/downloads), then clone the repo (download it to your computer):

```bash
git clone https://github.com/KasparasVe55/data-mining-project.git
cd data-mining-project
```

When git asks you to log in, sign in to your GitHub account (you need to have accepted the collaborator invite first).

### 2. Every time you start working

**Step 1: get the latest version of `main`**

```bash
git checkout main
git pull
```

**Step 2: create a feature branch** (your own copy to work on, so you don't break `main`)

```bash
git checkout -b your-branch-name
```

Use a short descriptive name, e.g. `add-cleaning-script` or `kasparas-eda`.

**Step 3: make your changes**

Add or edit files in `scripts/`, `outputs/` or `data/`.

**Step 4: commit your changes** (save a snapshot with a message)

```bash
git status                 # see which files changed
git add .                  # stage all changed files
git commit -m "Short description of what you did"
```

You can commit as many times as you like while working.

**Step 5: push your branch to GitHub**

```bash
git push -u origin your-branch-name
```

(After the first push of a branch, plain `git push` is enough.)

**Step 6: merge into `main` with a Pull Request**

1. Open the repo on GitHub. A yellow banner shows your recently pushed branch. Click **Compare & pull request**.
2. Add a short description and click **Create pull request**.
3. If there are no conflicts, click **Merge pull request**, then **Confirm merge**.
4. Back on your computer, switch to `main` and update it:

```bash
git checkout main
git pull
```

### Rules

- Never commit directly to `main`. Always use a branch and a Pull Request.
- Always `git pull` on `main` before creating a new branch.
- If you get a merge conflict and aren't sure what to do, ask in the group chat before forcing anything.
