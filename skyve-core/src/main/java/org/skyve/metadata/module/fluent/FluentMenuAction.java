package org.skyve.metadata.module.fluent;

import org.skyve.impl.metadata.repository.module.ActionMetaData;
import org.skyve.impl.metadata.repository.module.ApplicableTo;
import org.skyve.metadata.module.menu.MenuItem;

abstract class FluentMenuAction<T extends FluentMenuAction<T>> {
	protected FluentMenuAction() {
		// nothing to see here
	}

	@SuppressWarnings("unchecked")
	protected T from(MenuItem item) {
		name(item.getName());
		item.getUxUis().forEach(this::addUxUi);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T name(String name) {
		get().setName(name);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addUxUi(String uxui) {
		ApplicableTo applicable = new ApplicableTo();
		applicable.setUxUi(uxui);
		get().getUxuis().add(applicable);
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T removeUxUi(String uxui) {
		get().getUxuis().removeIf(u -> uxui.equals(u.getUxUi()));
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T clearUxUis() {
		get().getUxuis().clear();
		return (T) this;
	}

	public abstract ActionMetaData get();
}
