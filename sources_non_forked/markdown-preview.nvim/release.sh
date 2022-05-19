#!/usr/bin/env bash

set -e
[ "$TRACE" ] && set -x

# Create tag and push
tag="v$(node -p "require('./package.json').version")"
git tag -f "$tag" -m "Release $tag"
git push --tags

GH_API="https://api.github.com"
GH_REPO="$GH_API/repos/iamcco/markdown-preview.nvim"
GH_TAGS="$GH_REPO/releases/tags/$tag"
AUTH="Authorization: token $GITHUB_API_TOKEN"

echo "Creating release for $tag"
curl -X POST -H "Authorization: token $GITHUB_API_TOKEN" \
  -H "Content-Type: application/json" \
  --data "{\"tag_name\":\"$tag\"}" \
  "$GH_REPO/releases"

# upload assets
cd ./app/bin
tar -zcf markdown-preview-macos.tar.gz markdown-preview-macos
tar -zcf markdown-preview-linux.tar.gz markdown-preview-linux
zip markdown-preview-win.zip markdown-preview-win.exe

declare -a files=("markdown-preview-win.zip" "markdown-preview-macos.tar.gz" "markdown-preview-linux.tar.gz")

# Validate token.
curl -o /dev/null -sH "$AUTH" $GH_REPO || { echo "Error: Invalid repo, token or network issue!";  exit 1; }

# Read asset tags.
response=$(curl -sH "$AUTH" $GH_TAGS)

# Get ID of the asset based on given filename.
eval $(echo "$response" | grep -m 1 "id.:" | grep -w id | tr : = | tr -cd '[[:alnum:]]=')
[ "$id" ] || { echo "Error: Failed to get release id for tag: $tag"; echo "$response" | awk 'length($0)<100' >&2; exit 1; }

# Upload asset
for filename in "${files[@]}"
do
  GH_ASSET="https://uploads.github.com/repos/iamcco/markdown-preview.nvim/releases/$id/assets?name=$filename"
  echo "Uploading $filename"
  curl -X POST -H "Authorization: token $GITHUB_API_TOKEN" \
    -H "Content-Type: application/octet-stream" \
    --data-binary @"$filename" \
    $GH_ASSET
done

# clear bin
rm ./*
