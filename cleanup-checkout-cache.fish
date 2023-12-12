#!/usr/bin/env fish

# From
# https://guix.gnu.org/manual/devel/en/guix.html#index-Git_002c-using-the-latest-commit
# "Checkouts are kept in a cache under ~/.cache/guix/checkouts to speed up
#  consecutive accesses to the same repository. You may want to clean it up once
#  in a while to save disk space."
#
# This script prints out a list of shell script commands (active of commented
# out), and remarks which can be executed of further refined.

# The path must not be surrounded by double quotes, i.e. it must NOT be a
# string.
set checkoutCache ~/.cache/guix/checkouts

# regular expression for matching an URL
set regexp "\(https\|http\|file\)\?://[^ ]\+"
# printf "[DEBUG] regexp:\n%s\n" $regexp

### Be away that printing arrays in Fish can be tricky

# find items under $checkoutCache which are directories
set cacheDirs (find $checkoutCache -mindepth 1 -maxdepth 1 -type d)
# printf "[DEBUG] %s cacheDirs:\n" (count $cacheDirs)
# printf "%s\n" $cacheDirs

# find items under $checkoutCache which are NOT directories
set cacheNonDirs (find $checkoutCache -mindepth 1 -maxdepth 1 -not -type d)
# printf "[DEBUG] %s cacheNonDirs:\n" (count $cacheNonDirs)
# printf "%s\n" $cacheNonDirs

set allChannelRepos (guix system describe | \
                     grep repository | grep --only-matching "$regexp")
# printf "[DEBUG] %s allChannelRepos:\n" (count $allChannelRepos)
# printf "%s\n" (string join \n $allChannelRepos)

# Define empty variables which will serve as arrays. Can't use:
#    set <variable> ""  # some comment
# since that creates a variable containing an empty string.
set keepDirs       # local clones of git repositories actively used & cached
set keepURLs       # fetch-URLs of these clones
set removeDirs     # inactive clones ...
set removeURLs     # ... and theirs fetch-URLs
set others         # other, non-git-repository items in the checkout cache

for dir in $cacheDirs
    # printf "[DEBUG] dir: %s\n" $dir
    set dirGit $dir/.git
    if test -d $dirGit  # is ... a directory?
        set dirURL (git --git-dir=$dirGit remote --verbose | \
                    grep fetch | grep --only-matching "$regexp")
        # printf "[DEBUG] dirURL: %s\n" $dirURL
        if contains $dirURL $allChannelRepos
            set --append keepDirs $dir
            set --append keepURLs $dirURL
        else
            set --append removeDirs $dir
            set --append removeURLs $dirURL
        end
    else
        set --append others $dir
    end
end

# test -n "$<variable>":
# $<variable> surrounded by double quotes gets converted to a string. By
# testing if such a string has a non-zero length we ask if the $<variable> is
# a void value?

if test -n "$removeDirs"
    printf "### %s Cached checkouts to delete:\n" (count $removeDirs)
    # The array size of $removeDirs and $removeURLs are the same
    for i in (seq (count $removeDirs))
        # printf "%s # %s\n" $removeDirs[$i] $removeURLs[$i]
        # Don't delete it the straight away. Just mark it for deletetion by
        # renaming it. And even that in a dry-run. Better safe than sorry.
        # (Thank me later ;-)
        printf "mv %s{,.deleteMe}  # %s\n" $removeDirs[$i] $removeURLs[$i]
    end
end

if test -n "$keepDirs"
    printf "\n### %s Cached checkouts to keep:\n" (count $keepDirs)
    # The array size of $keepDirs and $keepURLs are the same
    for i in (seq (count $keepDirs))
        printf "# %s  # %s\n" $keepDirs[$i] $keepURLs[$i]
    end
end

if test -n "$cacheNonDirs"
    printf "\n### %s Non-directory items in the checkout cache directory:\n" (count $cacheNonDirs)
    for elem in $cacheNonDirs
        printf "# %s\n" $elem
    end
end
if test -n "$others"
    printf "\n### %s Other items in the checkout cache directory:\n" (count $others)
    for elem in $others
        printf "# %s\n" $elem
    end
end
