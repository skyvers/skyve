<!DOCTYPE html>
<html>
<head>
	<meta charset="UTF-8" />
	<title>${title}</title>
	<#include "/reportBaseStyles.ftlh">
	<style>
        @page {
            size: A4 landscape;
        }
    </style>
</head>
<body>
	<header>
		<div class="logo">
			<@image filename="logo.png" alt="Skyve Logo" height="40" />
		</div>
		<div class="company">
			<h2 class="name">${title}</h2>
		</div>
	</header>