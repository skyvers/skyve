package org.skyve.metadata.view.model;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.Attribute;
import org.skyve.wildcat.bind.BindUtil;
import org.skyve.wildcat.metadata.view.widget.bound.input.InputWidget;

public final class ComparisonProperty {
	private String name;
	private String title;
	private InputWidget widget;
	private Object newValue;
	private Object oldValue;

	public ComparisonProperty() {
		
	}
	
	public ComparisonProperty(String name,
								String title,
								InputWidget widget,
								Object newValue,
								Object oldValue) {
		this.name = name;
		this.title = title;
		this.widget = widget;
		this.newValue = newValue;
		this.oldValue = oldValue;
	}
	
	public ComparisonProperty(Attribute attribute,
								Bean oldBean,
								Bean newBean)
	throws Exception {
		name = attribute.getName();
		title = attribute.getDisplayName();
		widget = attribute.getDefaultInputWidget();
		if (oldBean != null) {
			oldValue = BindUtil.get(oldBean, name);
		}
		if (newBean != null) {
			newValue = BindUtil.get(newBean, name);
		}
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public InputWidget getWidget() {
		return widget;
	}
	public void setWidget(InputWidget widget) {
		this.widget = widget;
	}
	public Object getNewValue() {
		return newValue;
	}
	public void setNewValue(Object newValue) {
		this.newValue = newValue;
	}
	public Object getOldValue() {
		return oldValue;
	}
	public void setOldValue(Object oldValue) {
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
