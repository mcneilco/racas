## Building Multiarch r-repo image

### Using Manifest 

This proved to be the most efficient way as qemu has some serious lag issues with building multiarch packages.

On armd64 v8 system:
```
docker build --platform linux/arm64/v8 -t mcneilco/acas-r-repo:2023.1.0-linuxarm64v8 -f Dockerfile.repo .

docker push mcneilco/acas-r-repo:2023.1.0-linuxarm64v8
```

On amd64 system:
```
docker build --platform linux/amd64 -t mcneilco/acas-r-repo:2023.1.0-linuxamd64 -f Dockerfile.repo .

docker push mcneilco/acas-r-repo:2023.1.0-linuxamd64
```

On either system:

```
docker pull mcneilco/acas-r-repo:2023.1.0-linuxamd64 
docker pull mcneilco/acas-r-repo:2023.1.0-linuxarm64v8

docker manifest rm mcneilco/acas-r-repo:2023.1.0

docker manifest create \
mcneilco/acas-r-repo:2023.1.0 \
--amend mcneilco/acas-r-repo:2023.1.0-linuxamd64 \
--amend mcneilco/acas-r-repo:2023.1.0-linuxarm64v8

docker manifest push mcneilco/acas-r-repo:2023.1.0
```

### Using buildx multi platform builds

This is much simpler but qemu is extremely slow when building multiarch packages.

```
docker buildx build --push --platform linux/amd64,linux/arm64/v8 -t mcneilco/acas-r-repo:2023.1.0 -f Dockerfile.repo .
```


## Building Multiarch racas image from acas-r-repo

This is currently done via automated builds but this command should be equivalent to the current build process
```
docker buildx build --push --no-cache --platform linux/amd64,linux/arm64/v8 -t mcneilco/racas-oss:2023.1.0 .
```
