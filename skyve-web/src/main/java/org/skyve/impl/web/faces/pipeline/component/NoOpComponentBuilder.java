package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import javax.faces.component.UIComponent;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentImage;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.widget.bound.tabular.AbstractDataWidget;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.bound.FilterParameter;

public class NoOpComponentBuilder extends ComponentBuilder {
	@Override
	public UIComponent view(UIComponent component, String invisibleConditionName) {
		return component;
	}

	@Override
	public List<UIComponent> toolbars(List<UIComponent> components, String widgetId) {
		return components;
	}

	@Override
	public UIComponent tabPane(UIComponent component, TabPane tabPane) {
		return component;
	}

	@Override
	public UIComponent tab(UIComponent component, Tab tab) {
		return component;
	}

	@Override
	public UIComponent border(UIComponent component, String title, String invisibileConditionName, Integer pixelWidth) {
		return component;
	}

	@Override
	public UIComponent label(UIComponent component, String value) {
		return component;
	}

	@Override
	public UIComponent spacer(UIComponent component, Spacer spacer) {
		return component;
	}

	@Override
	public UIComponent actionButton(UIComponent component, 
										String listBinding,
										String listVar,
										Button button,
										Action action) {
		return component;
	}

	@Override
	public UIComponent reportButton(UIComponent component, Button button, Action action) {
		return component;
	}

	@Override
	public UIComponent downloadButton(UIComponent component,
										Button button,
										Action action,
										String moduleName,
										String documentName) {
		return component;
	}

	@Override
	public UIComponent staticImage(UIComponent component, StaticImage image) {
		return component;
	}

	@Override
	public UIComponent dynamicImage(UIComponent component, DynamicImage image, String moduleName, String documentName) {
		return component;
	}

	@Override
	public UIComponent blurb(UIComponent component, String listVar, String value, String binding, Blurb blurb) {
		return component;
	}

	@Override
	public UIComponent label(UIComponent component, String listVar, String value, String binding, Label label) {
		return component;
	}

	@Override
	public UIComponent dataGrid(UIComponent component, String listVar, DataGrid grid) {
		return component;
	}

	@Override
	public UIComponent dataRepeater(UIComponent component, String listVar, DataRepeater repeater) {
		return component;
	}

	@Override
	public UIComponent addDataGridBoundColumn(UIComponent component,
												UIComponent current,
												AbstractDataWidget widget,
												DataGridBoundColumn column,
												String listVar,
												String columnTitle,
												String columnBinding,
												StringBuilder gridColumnExpression) {
		return component;
	}

	@Override
	public UIComponent addedDataGridBoundColumn(UIComponent component, UIComponent current) {
		return component;
	}

	@Override
	public UIComponent addDataGridContainerColumn(UIComponent component,
													UIComponent current,
													AbstractDataWidget widget,
													DataGridContainerColumn column) {
		return component;
	}

	@Override
	public UIComponent addedDataGridContainerColumn(UIComponent component, UIComponent current) {
		return component;
	}

	@Override
	public UIComponent addDataGridActionColumn(UIComponent component,
													UIComponent current,
													DataGrid grid,
													String listVar,
													String gridColumnExpression,
													String singluarDocumentAlias,
													boolean inline) {
		return component;
	}

	@Override
	public UIComponent listGrid(UIComponent component,
									String modelDocumentName,
									String modelName,
									ListModel<? extends Bean> model,
									ListGrid listGrid,
									boolean canCreateDocument) {
		return component;
	}

	@Override
	public UIComponent listRepeater(UIComponent component,
										String modelDocumentName,
										String modelName,
										ListModel<? extends Bean> model,
										List<FilterParameter> filterParameters,
										String title,
										boolean showColumnHeaders,
										boolean showGrid) {
		return component;
	}

	@Override
	public UIComponent listMembership(UIComponent component, ListMembership membership) {
		return component;
	}

	@Override
	public UIComponent checkBox(UIComponent component,
									String listVar,
									CheckBox checkBox,
									String title,
									boolean required) {
		return component;
	}

	@Override
	public UIComponent colourPicker(UIComponent component,
										String listVar,
										ColourPicker colour,
										String title,
										boolean required) {
		return component;
	}

	@Override
	public UIComponent combo(UIComponent component, String listVar, Combo combo, String title, boolean required) {
		return component;
	}

	@Override
	public UIComponent contentImage(UIComponent component,
										String listVar,
										ContentImage image,
										String title,
										boolean required) {
		return component;
	}

	@Override
	public UIComponent contentLink(UIComponent component,
									String listVar,
									ContentLink link,
									String title,
									boolean required) {
		return component;
	}

	@Override
	public UIComponent html(UIComponent component, String listVar, HTML html, String title, boolean required) {
		return component;
	}

	@Override
	public UIComponent lookupDescription(UIComponent component,
											String listVar,
											LookupDescription lookup,
											String title,
											boolean required,
											String displayBinding,
											QueryDefinition query) {
		return component;
	}

	@Override
	public UIComponent password(UIComponent component,
									String listVar,
									Password password,
									String title,
									boolean required) {
		return component;
	}

	@Override
	public UIComponent radio(UIComponent component, String listVar, Radio radio, String title, boolean required) {
		return component;
	}

	@Override
	public UIComponent richText(UIComponent component, String listVar, RichText text, String title, boolean required) {
		return component;
	}

	@Override
	public UIComponent spinner(UIComponent component, String listVar, Spinner spinner, String title, boolean required) {
		return component;
	}

	@Override
	public UIComponent textArea(UIComponent component,
									String listVar,
									TextArea text,
									String title,
									boolean required,
									Integer length) {
		return component;
	}

	@Override
	public UIComponent text(UIComponent component,
								String listVar,
								TextField text,
								String title,
								boolean required,
								Integer length,
								Converter<?> converter,
								Format<?> format,
								javax.faces.convert.Converter facesConverter) {
		return component;
	}

	@Override
	public UIComponent actionLink(UIComponent component,
									String listBinding,
									String listVar,
									Link link,
									String actionName) {
		return component;
	}

	@Override
	public UIComponent report(UIComponent component, Action action) {
		return component;
	}

	@Override
	public UIComponent download(UIComponent component, Action action, String moduleName, String documentName) {
		return component;
	}

	@Override
	public UIComponent action(UIComponent component,
								String listBinding,
								String listVar,
								Action action,
								ImplicitActionName name,
								String title) {
		return component;
	}
}
