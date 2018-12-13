package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.web.faces.FacesUtil;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;

public class AdminFacesComponentBuilder extends NoOpComponentBuilder {
	private static final String BUTTON_STYLE_CLASS = "btn-primary";
	private static final String DATA_TABLE_STYLE_CLASS = "box-primary";
	
	@Override
	public UIComponent action(UIComponent component, String listBinding, String listVar, Action action, ImplicitActionName name, String title) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}

	@Override
	public UIComponent actionButton(UIComponent component, String listBinding, String listVar, Button button, Action action) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent download(UIComponent component, Action action, String moduleName, String documentName) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent downloadButton(UIComponent component, Button button, Action action, String moduleName, String documentName) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent report(UIComponent component, Action action) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent reportButton(UIComponent component, Button button, Action action) {
		FacesUtil.setStyleCLass(component, BUTTON_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent upload(UIComponent component, Action action) {
		styleUploadButton(component);
		return component;
	}

	@Override
	public UIComponent uploadButton(UIComponent component, Button button, Action action) {
		styleUploadButton(component);
		return component;
	}
	
	private static void styleUploadButton(UIComponent span) {
		if (span != null) {
			List<UIComponent> children = span.getChildren();
			if (children.size() >= 2) {
				FacesUtil.setStyleCLass(children.get(1), BUTTON_STYLE_CLASS);
			}
		}
	}
	
	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									ListGrid listGrid,
									boolean canCreateDocument) {
		FacesUtil.setStyleCLass(component, DATA_TABLE_STYLE_CLASS);
		return component;
	}
	
	@Override
	public UIComponent dataGrid(UIComponent component, String listVar, boolean ordered, DataGrid grid) {
		FacesUtil.setStyleCLass(component, DATA_TABLE_STYLE_CLASS);
		return component;
	}
}
