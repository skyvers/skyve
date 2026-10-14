package org.skyve.impl.web.faces.pipeline.component;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.domain.types.converters.Converter;
import org.skyve.domain.types.converters.Format;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.Geometry;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.input.LookupDescription;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.input.RichText;
import org.skyve.impl.metadata.view.widget.bound.input.Slider;
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
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.view.Action;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.faces.component.UIComponent;

public class NoOpComponentBuilder extends ComponentBuilder {
	@Override
	public @Nullable UIComponent view(@Nullable UIComponent component, boolean createView) {
		return component;
	}

	@Override
	public @Nullable List<UIComponent> toolbars(@Nullable List<UIComponent> components, @Nullable String widgetId) {
		return components;
	}

	@Override
	public @Nullable UIComponent tabPane(@Nullable UIComponent component,
											@Nonnull TabPane tabPane,
											@Nonnull String moduleName,
											@Nonnull String documentName) {
		return component;
	}

	@Override
	public @Nullable UIComponent tab(@Nullable UIComponent component, @Nullable String title, @Nonnull Tab tab) {
		return component;
	}

	@Override
	public @Nullable UIComponent tabPaneScript(@Nullable UIComponent component,
													@Nonnull TabPane tabPane,
													@Nonnull String moduleName,
													@Nonnull String documentName,
													@Nonnull String tabPaneComponentId) {
		return component;
	}

	@Override
	public @Nullable UIComponent sidebarScript(@Nullable UIComponent component,
													@Nonnull Sidebar sidebar,
													boolean createView,
													@Nonnull String sidebarComponentId) {
		return component;
	}
	
	@Override
	public @Nullable UIComponent border(@Nullable UIComponent component,
											@Nullable String title,
											@Nullable String invisibleCondition,
											@Nullable Integer pixelWidth,
											@Nullable Collapsible collapsible) {
		return component;
	}

	@Override
	public @Nullable UIComponent label(@Nullable UIComponent component, @Nonnull String value) {
		return component;
	}

	@Override
	public @Nullable UIComponent spacer(@Nullable UIComponent component, @Nonnull Spacer spacer) {
		return component;
	}

	/**
	 * Leaves dialog-button construction unchanged.
	 *
	 * @param component the current component
	 * @param label raw button label and nullable escape flag
	 * @param button dialog-button metadata
	 * @param formDisabledCondition optional form-level disabled condition
	 * @return the original component
	 */
	@Override
	public @Nullable UIComponent dialogButton(@Nullable UIComponent component,
													@Nullable EscapableText label,
													@Nonnull DialogButton button,
													@Nullable String formDisabledCondition) {
		return component;
	}

	@Override
	public @Nullable UIComponent zoomIn(@Nullable UIComponent component, 
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nonnull ZoomIn zoomIn,
													@Nullable String formDisabledCondition) {
		return component;
	}

	@Override
	public @Nullable UIComponent actionButton(@Nullable UIComponent component, 
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText, 
													@Nonnull Button button,
													@Nullable String formDisabledCondition,
													@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent reportButton(@Nullable UIComponent component,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText, 
													@Nonnull Button button,
													@Nullable String formDisabledCondition,
													@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent downloadButton(@Nullable UIComponent component,
													@Nullable String dataWidgetBinding,
													@Nullable String dataWidgetVar,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText, 
													@Nonnull Button button,
													@Nullable String formDisabledCondition,
													@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent uploadButton(@Nullable UIComponent component,
													@Nullable String label,
													@Nullable String iconStyleClass,
													@Nullable String toolTip,
													@Nullable String confirmationText, 
													@Nonnull Button button,
													@Nullable String formDisabledCondition,
													@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent staticImage(@Nullable UIComponent component, @Nonnull String fileUrl, @Nonnull StaticImage image) {
		return component;
	}

	@Override
	public @Nullable UIComponent dynamicImage(@Nullable UIComponent component, @Nonnull DynamicImage image, @Nonnull String moduleName, @Nonnull String documentName) {
		return component;
	}

	@Override
	public @Nullable UIComponent blurb(@Nullable UIComponent component, @Nullable String dataWidgetVar, @Nullable String value, @Nullable String binding, @Nonnull Blurb blurb) {
		return component;
	}

	@Override
	public @Nullable UIComponent label(@Nullable UIComponent component, @Nullable String dataWidgetVar, @Nullable String value, @Nullable String binding, @Nonnull Label label) {
		return component;
	}

	@Override
	public @Nullable UIComponent dataGrid(@Nullable UIComponent component, @Nonnull String dataWidgetVar, boolean ordered, @Nonnull DataGrid grid) {
		return component;
	}

	@Override
	public @Nullable UIComponent dataRepeater(@Nullable UIComponent component, @Nonnull String dataWidgetVar, @Nonnull DataRepeater repeater) {
		return component;
	}

	@Override
	public @Nullable UIComponent addDataGridBoundColumn(@Nullable UIComponent component,
															@Nonnull UIComponent current,
															@Nonnull AbstractDataWidget widget,
															@Nonnull DataGridBoundColumn column,
															@Nullable String dataWidgetVar,
															@Nullable String columnTitle,
															@Nonnull String columnBinding,
															@Nonnull StringBuilder gridColumnExpression,
															@Nullable HorizontalAlignment alignment,
															@Nullable Integer pixelWidth) {
		return component;
	}

	@Override
	public @Nullable UIComponent addedDataGridBoundColumn(@Nullable UIComponent component,
																@Nonnull UIComponent current,
																@Nonnull HorizontalAlignment alignment) {
		return component;
	}

	@Override
	public @Nullable UIComponent addDataGridContainerColumn(@Nullable UIComponent component,
																@Nonnull UIComponent current,
																@Nonnull AbstractDataWidget widget,
																@Nullable String title,
																@Nonnull DataGridContainerColumn column,
																@Nullable HorizontalAlignment alignment) {
		return component;
	}

	@Override
	public @Nullable UIComponent addedDataGridContainerColumn(@Nullable UIComponent component, @Nonnull UIComponent current) {
		return component;
	}

	@Override
	public @Nullable UIComponent addDataGridActionColumn(@Nullable UIComponent component,
																@Nonnull UIComponent current,
																@Nonnull DataGrid grid,
																@Nullable String dataWidgetVar,
																@Nonnull String gridColumnExpression,
																@Nullable String singluarDocumentAlias,
																boolean inline,
																boolean canCreate,
																boolean canDelete) {
		return component;
	}

	@Override
	public @Nullable UIComponent map(@Nullable UIComponent component,
										@Nonnull MapDisplay map,
										@Nonnull String moduleName,
										@Nonnull String queryName,
										@Nonnull String geometryBinding) {
		return component;
	}

	@Override
	public @Nullable UIComponent map(@Nullable UIComponent component, @Nonnull MapDisplay map, @Nonnull String modelName) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent geometry(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull Geometry geometry,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent geometryMap(@Nullable EventSourceComponent component,
															@Nonnull GeometryMap geometry,
															@Nullable String formDisabledCondition,
															@Nullable String title,
															@Nullable String requiredMessage) {
		return component;
	}
	
	@Override
	public @Nullable UIComponent chart(@Nullable UIComponent component, @Nonnull Chart chart) {
		return component;
	}

	@Override
	public @Nullable UIComponent listGrid(@Nullable UIComponent component,
												@Nonnull String moduleName,
												@Nullable String modelDocumentName,
												@Nullable String modelName,
												@Nonnull String uxui,
												@Nonnull ListModel<Bean> model,
												@Nullable Document owningDocument,
												@Nonnull ListGrid listGrid,
												@Nullable String stickyHeaderAnchorSelector,
												boolean aggregateQuery) {
		return component;
	}
	
	@Override
	public @Nullable UIComponent listGridContextMenu(@Nullable UIComponent component, @Nonnull String listGridId, @Nonnull ListGrid listGrid) {
		return component;
	}

	@Override
	public @Nullable UIComponent listRepeater(@Nullable UIComponent component,
													@Nullable String modelDocumentName,
													@Nullable String modelName,
													@Nonnull String uxui,
													@Nonnull ListModel<Bean> model,
													@Nullable List<FilterParameter> filterParameters,
													@Nullable List<Parameter> parameters,
													boolean showColumnHeaders,
													boolean showGrid) {
		return component;
	}

	/**
	 * Leaves list-membership caption construction unchanged for no-op builders.
	 *
	 * @param component the existing wrapper component
	 * @param candidatesHeading raw candidates-list heading and nullable escape flag;
	 *        {@code null} and {@code Boolean.TRUE} escape, while {@code Boolean.FALSE}
	 *        allows trusted markup
	 * @param membersHeading raw members-list heading and nullable escape flag;
	 *        {@code null} and {@code Boolean.TRUE} escape, while {@code Boolean.FALSE}
	 *        allows trusted markup
	 * @param membership the list-membership metadata
	 * @return {@code component} unchanged
	 */
	@Override
	public @Nullable EventSourceComponent listMembership(@Nullable EventSourceComponent component,
															@Nullable EscapableText candidatesHeading,
															@Nullable EscapableText membersHeading,
															@Nonnull ListMembership membership) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent checkBox(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull CheckBox checkBox,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent colourPicker(@Nullable EventSourceComponent component,
															@Nullable String dataWidgetVar,
															@Nonnull ColourPicker colour,
															@Nullable String formDisabledCondition,
															@Nullable String title,
															@Nullable String requiredMessage,
															@Nullable HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent combo(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull Combo combo,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable UIComponent content(@Nullable UIComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull ContentUpload content,
											@Nullable String formDisabledCondition,
											@Nullable String title,
											@Nullable String requiredMessage,
											@Nullable HorizontalAlignment textAlignment,
											boolean formContext,
											boolean imageUpload) {
		return component;
	}

	@Override
	public @Nullable UIComponent addContentSignature(@Nullable UIComponent component,
															@Nonnull UIComponent layout,
															@Nonnull ContentSignature signature,
															@Nullable String formDisabledCondition,
															@Nullable String title,
															@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable UIComponent html(@Nullable UIComponent component,
											@Nullable String dataWidgetVar,
											@Nonnull HTML html,
											@Nullable String formDisabledCondition,
											@Nullable String title,
											@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent lookupDescription(@Nullable EventSourceComponent component,
																@Nullable String dataWidgetVar,
																@Nonnull LookupDescription lookup,
																@Nullable String formDisabledCondition,
																@Nullable String title,
																@Nullable String requiredMessage,
																@Nullable HorizontalAlignment textAlignment,
																@Nonnull String displayBinding,
																@Nullable QueryDefinition query,
																@Nonnull Document owningDocument) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent password(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull Password password,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable HorizontalAlignment textAlignment) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent radio(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull Radio radio,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent richText(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull RichText text,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent spinner(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull Spinner spinner,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable HorizontalAlignment textAlignment,
														@Nullable jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent slider(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull Slider spinner,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent textArea(@Nullable EventSourceComponent component,
														@Nullable String dataWidgetVar,
														@Nonnull TextArea text,
														@Nullable String formDisabledCondition,
														@Nullable String title,
														@Nullable String requiredMessage,
														@Nullable HorizontalAlignment textAlignment,
														@Nullable Integer length) {
		return component;
	}

	@Override
	public @Nullable EventSourceComponent text(@Nullable EventSourceComponent component,
													@Nullable String dataWidgetVar,
													@Nonnull TextField text,
													@Nullable String formDisabledCondition,
													@Nullable String title,
													@Nullable String requiredMessage,
													@Nullable HorizontalAlignment textAlignment,
													@Nullable Integer length,
													@Nullable Converter<?> converter,
													@Nullable Format<?> format,
													@Nullable jakarta.faces.convert.Converter<?> facesConverter) {
		return component;
	}

	@Override
	public @Nullable UIComponent actionLink(@Nullable UIComponent component,
												@Nullable String dataWidgetBinding,
												@Nullable String dataWidgetVar,
												@Nullable String value,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText, 
												@Nonnull Link link,
												@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent report(@Nullable UIComponent component,
											@Nullable String label,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText, 
											@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent download(@Nullable UIComponent component,
												@Nullable String dataWidgetBinding,
												@Nullable String dataWidgetVar,
												@Nullable String label,
												@Nullable String iconStyleClass,
												@Nullable String toolTip,
												@Nullable String confirmationText, 
												@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent upload(@Nullable UIComponent component,
											@Nullable String label,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText, 
											@Nonnull Action action) {
		return component;
	}

	@Override
	public @Nullable UIComponent remove(@Nullable UIComponent component,
											@Nullable String label,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText, 
											@Nonnull Action action,
											boolean canDelete) {
		return component;
	}

	@Override
	public @Nullable UIComponent action(@Nullable UIComponent component,
											@Nullable String dataWidgetBinding,
											@Nullable String dataWidgetVar,
											@Nullable String label,
											@Nullable String iconStyleClass,
											@Nullable String toolTip,
											@Nullable String confirmationText, 
											@Nullable ImplicitActionName name,
											@Nonnull Action action) {
		return component;
	}
}
