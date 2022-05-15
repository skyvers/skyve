package org.skyve.metadata.model.document.fluent;

import org.skyve.metadata.model.Dynamic;

public class FluentDynamic {
	private Dynamic dynamic = null;
	
	public FluentDynamic() {
		dynamic = new Dynamic();
	}
	
	public FluentDynamic(Dynamic dynamic) {
		this.dynamic = dynamic;
	}

	public FluentDynamic from(@SuppressWarnings("hiding") Dynamic dynamic) {
		bizletClassName(dynamic.getBizletClassName());
		dataFactoryClassName(dynamic.getDataFactoryClassName());
		dynamic.getActions().forEach((k, v) -> addAction(k, v));
		dynamic.getImages().forEach((k, v) -> addImage(k, v));
		dynamic.getModels().forEach((k, v) -> addModel(k, v));
		return this;
	}
	
	public FluentDynamic bizletClassName(String bizletClassName) {
		dynamic.setBizletClassName(bizletClassName);
		return this;
	}

	public FluentDynamic dataFactoryClassName(String dataFactoryClassName) {
		dynamic.setDataFactoryClassName(dataFactoryClassName);
		return this;
	}

	public FluentDynamic addAction(String name, String fullyQualifiedClassName) {
		dynamic.getActions().put(name, fullyQualifiedClassName);
		return this;
	}

	public FluentDynamic removeAction(String name) {
		dynamic.getActions().remove(name);
		return this;
	}

	public FluentDynamic addImage(String name, String fullyQualifiedClassName) {
		dynamic.getImages().put(name, fullyQualifiedClassName);
		return this;
	}

	public FluentDynamic removeImage(String name) {
		dynamic.getImages().remove(name);
		return this;
	}

	public FluentDynamic addModel(String name, String fullyQualifiedClassName) {
		dynamic.getModels().put(name, fullyQualifiedClassName);
		return this;
	}

	public FluentDynamic removeModel(String name) {
		dynamic.getModels().remove(name);
		return this;
	}

	public Dynamic get() {
		return dynamic;
	}
}
