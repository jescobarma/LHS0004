srclib <<- "/Library/Srclib/R"
source(file.path(srclib, "init.R", fsep=.Platform$file.sep))

# Run from scripts/LHS000402 (paths in bbdesign are relative: ../../data/derived)

bbdesign(name="bbdesign")

