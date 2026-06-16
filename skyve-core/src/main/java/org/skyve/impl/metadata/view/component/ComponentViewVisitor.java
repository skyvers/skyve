package org.skyve.impl.metadata.view.component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.ActionImpl;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.ViewVisitor;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.event.Addable;
import org.skyve.impl.metadata.view.event.Changeable;
import org.skyve.impl.metadata.view.event.Editable;
import org.skyve.impl.metadata.view.event.EventAction;
import org.skyve.impl.metadata.view.event.EventSource;
import org.skyve.impl.metadata.view.event.Focusable;
import org.skyve.impl.metadata.view.event.Removable;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.Selectable;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.Blurb;
import org.skyve.impl.metadata.view.widget.Button;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ProgressBar;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ContentSignature;
import org.skyve.impl.metadata.view.widget.bound.input.ContentUpload;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.SerializableMetaData;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;

/**
 * Rewrites component metadata so it can be embedded safely in an owning view context.
 *
 * <p>This visitor prefixes bindings, remaps action and condition names through the
 * supplied component-name map, and optionally narrows traversal output to a targeted
 * widget id.
 *
 * <p>Threading: not thread-safe; intended for one traversal instance per conversion.
 */
public class ComponentViewVisitor extends ViewVisitor {
	private String bindingPrefix;
	private Map<String, String> names;
	private String widgetId;
	
	/**
	 * Creates a visitor that rewrites component metadata into the owning view context.
	 *
	 * @param customer the active customer context
	 * @param module the active module context
	 * @param document the active document context
	 * @param view the destination view receiving converted metadata
	 * @param currentUxUi the active UX/UI profile
	 * @param bindingPrefix optional binding prefix applied to bound expressions
	 * @param names mapping from component-local names to owning-view names
	 * @param widgetId optional widget id used to extract a single targeted fragment
	 */
	@SuppressWarnings("java:S107") // Long parameter list preserves the existing framework/API contract.
	public ComponentViewVisitor(CustomerImpl customer, 
									ModuleImpl module, 
									DocumentImpl document, 
									ViewImpl view,
									String currentUxUi,
									String bindingPrefix,
									List<ComponentNameMap> names,
									String widgetId) {
		super(customer, module, document, view, currentUxUi);
		setUseMetaData(false);
		this.bindingPrefix = bindingPrefix;
		this.names = new TreeMap<>();
		for (ComponentNameMap name : names) {
			this.names.put(name.getFromComponent(), name.getMappedTo());
		}
		this.widgetId = widgetId;
	}
	
	private Identifiable identifiable;
	
	/**
	 * Returns the converted metadata payload from this traversal.
	 *
	 * <p>When a target {@code widgetId} is supplied, this returns that single matched
	 * element and fails if no match was discovered. Otherwise the full converted view
	 * content is returned.
	 *
	 * @return converted metadata elements to persist in the owning view
	 * @throws MetaDataException if a target widget id was requested but not found
	 */
	public List<SerializableMetaData> getContained() {
		if (identifiable == null) {
			if (widgetId != null) {
				throw new MetaDataException(String.format("Component definition with widgetId %s is not a valid widgetId.", widgetId));
			}
			return view.getContained();
		}
		
		List<SerializableMetaData> result = new ArrayList<>(1);
		result.add(identifiable);
		return result;
	}
	
	/**
	 * No-op; no view-level transformation is required for component embedding.
	 */
	@Override
	public void visitView() {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedView() {
		// nothing to do here
	}

	/**
	 * Prefixes the selected-tab-index binding, translates disable and invisible
	 * condition names, and captures the tab pane if it matches the target widget id.
	 *
	 * @param tabPane the tab pane being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		tabPane.setSelectedTabIndexBinding(prefixBinding(tabPane.getSelectedTabIndexBinding()));
		disable(tabPane);
		invisible(tabPane);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(tabPane.getWidgetId()))) {
			identifiable = tabPane;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedTabPane(TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the disable and invisible condition names of the tab.
	 *
	 * @param tab the tab being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		disable(tab);
		invisible(tab);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedTab(Tab tab, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the invisible condition name and captures the vbox if it matches
	 * the target widget id.
	 *
	 * @param vbox the vbox being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		invisible(vbox);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(vbox.getWidgetId()))) {
			identifiable = vbox;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedVBox(VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the invisible condition name and captures the hbox if it matches
	 * the target widget id.
	 *
	 * @param hbox the hbox being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		invisible(hbox);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(hbox.getWidgetId()))) {
			identifiable = hbox;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedHBox(HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the disable and invisible condition names, normalises the label layout
	 * when the owning module declares top-defined forms, and captures the form if it
	 * matches the target widget id.
	 *
	 * @param form the form being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitForm(Form form, boolean parentVisible, boolean parentEnabled) {
		disable(form);
		invisible(form);

		// If we have a top-defined module and this form is defaulting, 
		// explicitly set it to top for the component in case the component
		// belongs to a view in a side-defined module.
		// Note that top defined forms cannot be converted to side defined,
		// but side defined forms can be converted to top defined.
		// Thus any top-defined forms need to be made explicit when used in a component.
		if ((form.getLabelLayout() == null) && (module.getFormLabelLayout() == FormLabelLayout.top)) {
			form.setLabelLayout(FormLabelLayout.top);
		}

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(form.getWidgetId()))) {
			identifiable = form;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedForm(Form form, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitFormColumn(FormColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedFormItem(FormItem item, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedFormRow(FormRow row, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the button's action name through the component name map.
	 *
	 * @param button the button being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitButton(Button button, boolean parentVisible, boolean parentEnabled) {
		button.setActionName(translate(button.getActionName()));
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param zoomIn the zoom-in widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitZoomIn(ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
		bound(zoomIn);
		disable(zoomIn);
		invisible(zoomIn);
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param geometry the geometry widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		bound(geometry);
		disable(geometry);
		invisible(geometry);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedGeometry(Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param geometry the geometry map widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		bound(geometry);
		disable(geometry);
		invisible(geometry);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedGeometryMap(GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}
	
	/**
	 * Translates the invisible condition name.
	 *
	 * @param map the map display widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitMap(MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		invisible(map);
	}

	/**
	 * Translates the invisible condition name.
	 *
	 * @param chart the chart widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitChart(Chart chart, boolean parentVisible, boolean parentEnabled) {
		invisible(chart);
	}

	/**
	 * Translates the disable and invisible condition names.
	 *
	 * @param button the dialog button being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitDialogButton(DialogButton button, boolean parentVisible, boolean parentEnabled) {
		disable(button);
		invisible(button);
	}

	/**
	 * Translates the invisible condition name.
	 *
	 * @param image the dynamic image widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitDynamicImage(DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		invisible(image);
	}

	/**
	 * Translates the invisible condition name.
	 *
	 * @param spacer the spacer widget being visited
	 */
	@Override
	public void visitSpacer(Spacer spacer) {
		invisible(spacer);
	}

	/**
	 * Translates the invisible condition name.
	 *
	 * @param image the static image widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitStaticImage(StaticImage image, boolean parentVisible, boolean parentEnabled) {
		invisible(image);
	}

	/**
	 * Translates the invisible condition name and prefixes binding expressions
	 * within the link value.
	 *
	 * @param link the link widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitLink(Link link, boolean parentVisible, boolean parentEnabled) {
		invisible(link);
		link.setValue(prefixExpression(link.getLocalisedValue()));
	}

	/**
	 * Translates the invisible condition name and prefixes binding expressions
	 * within the blurb markup.
	 *
	 * @param blurb the blurb widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitBlurb(Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		invisible(blurb);
		blurb.setMarkup(prefixExpression(blurb.getLocalisedMarkup()));
	}

	/**
	 * Prefixes the binding, translates the invisible condition name, prefixes the
	 * {@code for} target binding and any binding expressions within the label value.
	 *
	 * @param label the label widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitLabel(Label label, boolean parentVisible, boolean parentEnabled) {
		bound(label);
		invisible(label);
		label.setFor(prefixBinding(label.getFor()));
		label.setValue(prefixExpression(label.getLocalisedValue()));
	}

	/**
	 * Prefixes the binding and translates the invisible condition name.
	 *
	 * @param progressBar the progress bar widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitProgressBar(ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		bound(progressBar);
		invisible(progressBar);
	}

	/**
	 * Translates the disable and invisible condition names, translates all
	 * condition names used for row-level actions, and prefixes the selected-id binding.
	 *
	 * @param grid the list grid being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		disable(grid);
		invisible(grid);
		grid.setDisableAddConditionName(translate(grid.getDisableAddConditionName()));
		grid.setDisableEditConditionName(translate(grid.getDisableEditConditionName()));
		grid.setDisableRemoveConditionName(translate(grid.getDisableRemoveConditionName()));
		grid.setDisableZoomConditionName(translate(grid.getDisableZoomConditionName()));
		grid.setPostRefreshConditionName(translate(grid.getPostRefreshConditionName()));
		grid.setSelectedIdBinding(prefixBinding(grid.getSelectedIdBinding()));
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedListGrid(ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Applies all {@link #visitListGrid} transformations and additionally prefixes
	 * the root-id binding.
	 *
	 * @param grid the tree grid being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		visitListGrid(grid, parentVisible, parentEnabled);
		grid.setRootIdBinding(prefixBinding(grid.getRootIdBinding()));
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedTreeGrid(TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the invisible condition name and the post-refresh condition name.
	 *
	 * @param repeater the list repeater being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitListRepeater(ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		invisible(repeater);
		repeater.setPostRefreshConditionName(translate(repeater.getPostRefreshConditionName()));
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedListRepeater(ListRepeater grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding, translates the disable and invisible condition names,
	 * translates row-level action condition names and the selected-id binding,
	 * and captures the data grid if it matches the target widget id.
	 *
	 * @param grid the data grid being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		bound(grid);
		disable(grid);
		invisible(grid);
		grid.setDisableAddConditionName(translate(grid.getDisableAddConditionName()));
		grid.setDisableEditConditionName(translate(grid.getDisableEditConditionName()));
		grid.setDisableRemoveConditionName(translate(grid.getDisableRemoveConditionName()));
		grid.setDisableZoomConditionName(translate(grid.getDisableZoomConditionName()));
		grid.setSelectedIdBinding(prefixBinding(grid.getSelectedIdBinding()));

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(grid.getWidgetId()))) {
			identifiable = grid;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedDataGrid(DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding, translates the invisible condition name, and captures
	 * the data repeater if it matches the target widget id.
	 *
	 * @param repeater the data repeater being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		bound(repeater);
		invisible(repeater);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(repeater.getWidgetId()))) {
			identifiable = repeater;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedDataRepeater(DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedDataGridBoundColumn(DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedDataGridContainerColumn(DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param checkBox the check box widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		bound(checkBox);
		disable(checkBox);
		invisible(checkBox);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedCheckBox(CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param membership the check membership widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		bound(membership);
		disable(membership);
		invisible(membership);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedCheckMembership(CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param colour the colour picker widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		bound(colour);
		disable(colour);
		invisible(colour);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedColourPicker(ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param combo the combo widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		bound(combo);
		disable(combo);
		invisible(combo);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedCombo(Combo combo, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Rewrites content upload bindings and conditions for the component context.
	 *
	 * @param content the content upload to rewrite; must not be {@code null}
	 * @param parentVisible whether ancestor metadata is visible
	 * @param parentEnabled whether ancestor metadata is enabled
	 */
	@Override
	public void visitContent(@Nonnull ContentUpload content, boolean parentVisible, boolean parentEnabled) {
		bound(content);
		disable(content);
		invisible(content);
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param signature the content signature widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitContentSignature(ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		bound(signature);
		disable(signature);
		invisible(signature);
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param html the HTML widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitHTML(HTML html, boolean parentVisible, boolean parentEnabled) {
		bound(html);
		disable(html);
		invisible(html);
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param membership the list membership widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		bound(membership);
		disable(membership);
		invisible(membership);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedListMembership(ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param comparison the comparison widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitComparison(Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		bound(comparison);
		disable(comparison);
		invisible(comparison);
	}

	/**
	 * Prefixes the binding, translates the disable and invisible condition names,
	 * and translates action-level condition names (add, clear, edit, pick).
	 *
	 * @param lookup the lookup description widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		bound(lookup);
		disable(lookup);
		invisible(lookup);
		lookup.setDisableAddConditionName(translate(lookup.getDisableAddConditionName()));
		lookup.setDisableClearConditionName(translate(lookup.getDisableClearConditionName()));
		lookup.setDisableEditConditionName(translate(lookup.getDisableEditConditionName()));
		lookup.setDisablePickConditionName(translate(lookup.getDisablePickConditionName()));
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedLookupDescription(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param password the password widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		bound(password);
		disable(password);
		invisible(password);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedPassword(Password password, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param radio the radio widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		bound(radio);
		disable(radio);
		invisible(radio);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedRadio(Radio radio, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param richText the rich text widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		bound(richText);
		disable(richText);
		invisible(richText);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedRichText(RichText richText, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param slider the slider widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		bound(slider);
		disable(slider);
		invisible(slider);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedSlider(Slider slider, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param spinner the spinner widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		bound(spinner);
		disable(spinner);
		invisible(spinner);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedSpinner(Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param text the text area widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		bound(text);
		disable(text);
		invisible(text);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedTextArea(TextArea text, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding and translates the disable and invisible condition names.
	 *
	 * @param text the text field widget being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		bound(text);
		disable(text);
		invisible(text);
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Prefixes the binding for every {@link InjectBinding} declared by the inject element.
	 *
	 * @param inject the inject element being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitInject(Inject inject, boolean parentVisible, boolean parentEnabled) {
		for (InjectBinding binding : inject.getBindings()) {
			bound(binding);
		}
	}

	/**
	 * Translates action names in the widget's on-changed event action list.
	 *
	 * @param changeable the widget whose on-changed handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		event(changeable.getChangedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnChangedEventHandler(Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-focus event action list.
	 *
	 * @param blurable the widget whose on-focus handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		event(blurable.getFocusActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnFocusEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-blur event action list.
	 *
	 * @param blurable the widget whose on-blur handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		event(blurable.getBlurActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnBlurEventHandler(Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-added event action list.
	 *
	 * @param addable the widget whose on-added handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		event(addable.getAddedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnAddedEventHandler(Addable addable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-edited event action list.
	 *
	 * @param editable the widget whose on-edited handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		event(editable.getEditedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnEditedEventHandler(Editable editable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-removed event action list.
	 *
	 * @param removable the widget whose on-removed handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		event(removable.getRemovedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnRemovedEventHandler(Removable removable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the widget's on-selected event action list.
	 *
	 * @param selectable the widget whose on-selected handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		event(selectable.getSelectedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnSelectedEventHandler(Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the lookup's on-picked event action list.
	 *
	 * @param lookup the lookup widget whose on-picked handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		event(lookup.getPickedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnPickedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates action names in the lookup's on-cleared event action list.
	 *
	 * @param lookup the lookup widget whose on-cleared handler is being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		event(lookup.getClearedActions());
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedOnClearedEventHandler(LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * No-op; re-render event actions carry no name that requires translation.
	 */
	@Override
	public void visitRerenderEventAction(RerenderEventAction rerender, EventSource source, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}

	/**
	 * Translates the server-side action name through the component name map.
	 *
	 * @param server the server-side action event action being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitServerSideActionEventAction(ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		server.setActionName(translate(server.getActionName()));
	}

	/**
	 * Prefixes the binding and translates the disable condition name.
	 *
	 * @param setDisabled the set-disabled event action being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitSetDisabledEventAction(SetDisabledEventAction setDisabled, boolean parentVisible, boolean parentEnabled) {
		bound(setDisabled);
		disable(setDisabled);
	}

	/**
	 * Prefixes the binding and translates the invisible condition name.
	 *
	 * @param setInvisible the set-invisible event action being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitSetInvisibleEventAction(SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
		bound(setInvisible);
		invisible(setInvisible);
	}

	/**
	 * Prefixes the binding of the toggle-disabled event action.
	 *
	 * @param toggleDisabled the toggle-disabled event action being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitToggleDisabledEventAction(ToggleDisabledEventAction toggleDisabled, boolean parentVisible, boolean parentEnabled) {
		bound(toggleDisabled);
	}

	/**
	 * Prefixes the binding of the toggle-visibility event action.
	 *
	 * @param toggleVisibility the toggle-visibility event action being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitToggleVisibilityEventAction(ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
		bound(toggleVisibility);
	}

	/**
	 * No-op; component embedding does not generate view-level actions.
	 */
	@Override
	public void visitCustomAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitAddAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitRemoveAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitZoomOutAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitNavigateAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitOKAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitSaveAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitCancelAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitDeleteAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitReportAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitBizExportAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitBizImportAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitDownloadAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitUploadAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitNewAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitEditAction(ActionImpl action) {
		// nothing to do here
	}

	/**
	 * Prefixes the value binding and any binding expressions within the value
	 * of the parameter.
	 *
	 * @param parameter the parameter being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitParameter(Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		if (parameter instanceof ParameterImpl p) {
			p.setValueBinding(prefixBinding(parameter.getValueBinding()));
			p.setValue(prefixExpression(parameter.getValue()));
		}
	}

	/**
	 * Prefixes the value binding and any binding expressions within the value
	 * of the filter parameter.
	 *
	 * @param parameter the filter parameter being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitFilterParameter(FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		if (parameter instanceof FilterParameterImpl p) {
			p.setValueBinding(prefixBinding(parameter.getValueBinding()));
			p.setValue(prefixExpression(parameter.getValue()));
		}
	}

	/**
	 * This is overridden to not visit the default widget as per the document attribute specification,
	 * but instead just changes the binding used.
	 */
	@Override
	public void visitDefaultWidget(DefaultWidget widget, boolean parentVisible, boolean parentEnabled) {
		bound(widget);
	}
	
	/**
	 * Prefixes the binding of a bound element with the component binding prefix.
	 *
	 * @param bound the bound element whose binding is to be prefixed
	 */
	private void bound(Bound bound) {
		bound.setBinding(prefixBinding(bound.getBinding()));
	}

	/**
	 * Translates the disabled condition name of a disableable element through
	 * the component name map.
	 *
	 * @param disableable the element whose disabled condition name is to be translated
	 */
	private void disable(Disableable disableable) {
		disableable.setDisabledConditionName(translate(disableable.getDisabledConditionName()));
	}
	
	/**
	 * Translates the invisible condition name of an invisible element through
	 * the component name map.
	 *
	 * @param invisible the element whose invisible condition name is to be translated
	 */
	private void invisible(Invisible invisible) {
		invisible.setInvisibleConditionName(translate(invisible.getInvisibleConditionName()));
	}

	/**
	 * Translates action names in a list of event actions through the component
	 * name map. Only {@link ServerSideActionEventAction} entries carry translatable names.
	 *
	 * @param actions the event actions to translate
	 */
	private void event(List<EventAction> actions) {
		for (EventAction action : actions) {
			if (action instanceof ServerSideActionEventAction server) {
				server.setActionName(translate(server.getActionName()));
			}
		}
	}
	
	/**
	 * Prefixes all binding references embedded in a message expression with the
	 * component binding prefix. Returns the expression unchanged when no prefix
	 * is configured.
	 *
	 * @param expression the message expression to rewrite, may be {@code null}
	 * @return the rewritten expression, or {@code null} if the input was {@code null}
	 */
	private String prefixExpression(String expression) {
		if (bindingPrefix == null) {
			return expression;
		}
		return BindUtil.prefixMessageExpressions(expression, bindingPrefix);
	}

	/**
	 * Maps a component-local condition or action name to the corresponding
	 * owning-view name via the component name map. When the name is not found
	 * directly, the negated form is tried and the negation of its mapped value
	 * is returned. Returns the original name unchanged when no mapping exists.
	 *
	 * @param name the condition or action name to translate, may be {@code null}
	 * @return the translated name, or {@code null} if the input was {@code null}
	 */
	private String translate(String name) {
		if (name == null) {
			return null;
		}
		String result = names.get(name);
		if (result == null) {
			// if we have a not condition, lookup the negation of it
			// and use the negation of the mapping.
			if (name.startsWith("not")) {
				String condish = BindUtil.negateCondition(name);
				result = names.get(condish);
				if (result == null) { // not a mapped negated condition - not found
					result = name;
				}
				else { // negate the mapped negated condition
					result = BindUtil.negateCondition(result);
				}
			}
			else { // not found
				result = name;
			}
		}
		return result;
	}
	
	/**
	 * Prepends the component binding prefix to a plain binding string.
	 * Returns the binding unchanged when no prefix is configured, and
	 * returns {@code null} when the binding is {@code null}.
	 *
	 * @param binding the binding string to prefix, may be {@code null}
	 * @return the prefixed binding, or {@code null} if the input was {@code null}
	 */
	private String prefixBinding(String binding) {
		if (binding == null) {
			return null;
		}
		if (bindingPrefix == null) {
			return binding;
		}
		return bindingPrefix + '.' + binding;
	}

	/**
	 * Translates the invisible condition name and captures the sidebar if it matches
	 * the target widget id.
	 *
	 * @param sidebar the sidebar being visited
	 * @param parentVisible whether the parent container is visible
	 * @param parentEnabled whether the parent container is enabled
	 */
	@Override
	public void visitSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		invisible(sidebar);

		// capture the targeted widget, if applicable
		if ((widgetId != null) && (widgetId.equals(sidebar.getWidgetId()))) {
			identifiable = sidebar;
		}
	}

	/**
	 * No-op.
	 */
	@Override
	public void visitedSidebar(Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// nothing to do here
	}
}
