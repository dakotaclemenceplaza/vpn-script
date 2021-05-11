build:
	stack ghc \
	--package typed-process \
	--package aeson \
	--package lens \
	--package lens-aeson \
	--package fdo-notify \
	--package http-conduit \
	-- -o vpn vpn.hs
