# RESQUE Profile App
A website that generates a profile from the RESQUE JSON.

Multiple files can be uploaded.

Needs to run on a Shiny Server.

Currently runs at [https://shiny.psy.lmu.de/felix/RESQUE_profile/](https://shiny.psy.lmu.de/felix/RESQUE_profile/)

**Internal note: Update packages on server**

*(Yes, this could be done in a proper CI/CD pipeline ...)*

Don't forget to increase the version numbers!

Run locally:

```bash
# Build and push OAmetrics package
cd ~/Documents/Github/RESQUE-Framework/OAmetrics
Rscript -e 'pkgdown:::build_site()'
git commit -a -m 'Auto-upload to server'
git push -u origin main

# Build and push RESQUER package
cd ~/Documents/Github/RESQUE-Framework/RESQUER
Rscript -e 'pkgdown:::build_site()'
git commit -a -m 'Auto-upload to server'
git push -u origin main
```

Run on the server:

```bash
Rscript -e 'remotes::install_github("nicebread/OAmetrics"); remotes::install_github("RESQUE-Framework/RESQUER", auth_token=NULL)'
systemctl restart shiny-server
```

Check if the package version have been updated: [https://shiny.psy.lmu.de/felix/RESQUE_profile/](https://shiny.psy.lmu.de/felix/RESQUE_profile/)

