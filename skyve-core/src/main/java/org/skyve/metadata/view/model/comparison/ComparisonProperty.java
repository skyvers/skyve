package org.skyve.metadata.view.model.comparison;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
import org.skyve.metadata.model.Attribute;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public final class ComparisonProperty {
	private String name;
	private String title;
	private InputWidget widget;
	private Object newValue;
	private Object oldValue;

	public ComparisonProperty() {
		
	}
	
	public ComparisonProperty(@Nullable String name,
								@Nullable String title,
								@Nullable InputWidget widget,
								@Nullable Object newValue,
								@Nullable Object oldValue) {
		this.name = name;
		this.title = title;
		this.widget = widget;
		this.newValue = newValue;
		this.oldValue = oldValue;
	}
	
	public ComparisonProperty(@Nonnull Attribute attribute,
								@Nullable Bean oldBean,
								@Nullable Bean newBean) {
		name = attribute.getName();
		title = attribute.getLocalisedDisplayName();
		widget = attribute.getDefaultInputWidget();
		if (oldBean != null) {
			oldValue = BindUtil.get(oldBean, name);
		}
		if (newBean != null) {
			newValue = BindUtil.get(newBean, name);
		}
	}

	public @Nullable String getName() {
		return name;
	}
	public void setName(@Nullable String name) {
		this.name = name;
	}
	public @Nullable String getTitle() {
		return title;
	}
	public void setTitle(@Nullable String title) {
		this.title = title;
	}
	public @Nullable InputWidget getWidget() {
		return widget;
	}
	public void setWidget(@Nullable InputWidget widget) {
		this.widget = widget;
	}
	public @Nullable Object getNewValue() {
		return newValue;
	}
	public void setNewValue(@Nullable Object newValue) {
		this.newValue = newValue;
	}
	public @Nullable Object getOldValue() {
		return oldValue;
	}
	public void setOldValue(@Nullable Object oldValue) {
		this.oldValue = oldValue;
	}
	public boolean isDirty() {
		boolean result = false;
		
		if (oldValue != null) {
			result = (! oldValue.equals(newValue));
		}
		else if (newValue != null) {
			result = (! newValue.equals(oldValue));
		}

		return result;
	}
}
