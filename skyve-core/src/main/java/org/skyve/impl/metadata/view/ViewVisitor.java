package org.skyve.impl.metadata.view;

import java.util.List;

import org.skyve.domain.Bean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
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
import org.skyve.impl.metadata.view.widget.Link;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.Label;
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
import org.skyve.impl.metadata.view.widget.bound.input.InputWidget;
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
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TabularColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.view.Disableable;
import org.skyve.metadata.view.Invisible;
import org.skyve.metadata.view.widget.bound.Bound;
import org.skyve.util.Binder.TargetMetaData;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Abstract visitor that traverses the full widget and action tree of a
 * {@link ViewImpl}, visiting every widget type and every action.
 *
 * <p>Traversal is depth-first and follows metadata declaration order. Entry
 * callbacks named {@code visit*} run before nested metadata; matching
 * {@code visited*} callbacks run after nested metadata. The supplied
 * {@code parentVisible} and {@code parentEnabled} flags describe the resolved
 * ancestor state, allowing subclasses to combine it with the current element.
 * Actions and event actions are dispatched through their type-specific callbacks.
 *
 * <p>Subclasses must implement the full callback surface. Use
 * {@link NoOpViewVisitor} when only selected metadata types are relevant.
 *
 * <p>Threading: not thread-safe; one instance per traversal.
 *
 * @see ActionVisitor
 * @see NoOpViewVisitor
 */
@SuppressWarnings("java:S6539") // Monster class but should stay cohesive
public abstract class ViewVisitor extends ActionVisitor {
	/**
	 * Logger associated with the concrete visitor type.
	 * NB An instance member LOGGER is OK here as this is not Serializable.
	 */
	@SuppressWarnings("java:S116") // LOGGER is OK as we treat it like a constant
	protected final Logger LOGGER = SkyveLoggerFactory.getLogger(getClass());

	/** Customer supplying localised metadata and overrides for this traversal. */
	protected @Nonnull CustomerImpl customer;
	/** Module owning the traversed view. */
	protected @Nonnull ModuleImpl module;
	/** Document against which bindings and default widgets are resolved. */
	protected @Nonnull DocumentImpl document;
	/** Root view traversed by {@link #visit()}. */
	protected @Nonnull ViewImpl view;
	/** UX/UI profile used to resolve component fragments. */
	protected @Nonnull String currentUxUi; // for resolving components
	/**
	 * Whether data-widget columns resolve default inputs from target metadata.
	 * Use TargetMetaData to resolve default widgets in data grids/repeaters
	 */
	// Note - we don't want to do this when adding binding prefixes to component fragments
	protected boolean useMetaData = true;
	
	/**
	 * Creates a visitor for one resolved customer, module, document, view, and UX/UI context.
	 *
	 * @param customer the customer supplying metadata overrides; must not be null
	 * @param module the module owning the view; must not be null
	 * @param document the document owning the view; must not be null
	 * @param view the view metadata to traverse; must not be null
	 * @param currentUxUi the UX/UI profile used to resolve component fragments; must not be null
	 */
	protected ViewVisitor(@Nonnull CustomerImpl customer,
							@Nonnull ModuleImpl module,
							@Nonnull DocumentImpl document,
							@Nonnull ViewImpl view,
							@Nonnull String currentUxUi) {
		this.customer = customer;
		this.module = module;
		this.document = document;
		this.view = view;
		this.currentUxUi = currentUxUi;
	}

	/**
	 * Traverses the complete root view, including contained widgets, sidebar metadata, and actions.
	 */
	public final void visit() {
		visitContainer(view, true, true);
	}
	
	/**
	 * Sets whether data-widget columns resolve their default inputs from target metadata.
	 *
	 * <p>Disabling resolution makes unresolved columns fall back to the business-key
	 * input widget, which is useful while rewriting component-fragment bindings.
	 *
	 * @param useMetaData whether target metadata should determine default input widgets
	 */
	protected final void setUseMetaData(boolean useMetaData) {
		this.useMetaData = useMetaData;
	}

	/**
	 * Begins visitation of the root view before any contained metadata.
	 */
	public abstract void visitView();

	/**
	 * Completes visitation of the root view after contained metadata and actions.
	 */
	public abstract void visitedView();

	/**
	 * Visits the tab pane in the current traversal context.
	 *
	 * @param tabPane the tab pane metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitTabPane(@Nonnull TabPane tabPane,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the tab pane after its child metadata.
	 *
	 * @param tabPane the tab pane metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedTabPane(@Nonnull TabPane tabPane,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the tab in the current traversal context.
	 *
	 * @param tab the tab metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitTab(@Nonnull Tab tab,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Completes visitation of the tab after its child metadata.
	 *
	 * @param tab the tab metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedTab(@Nonnull Tab tab,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the vertical box in the current traversal context.
	 *
	 * @param vbox the vbox metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitVBox(@Nonnull VBox vbox,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Completes visitation of the vertical box after its child metadata.
	 *
	 * @param vbox the vbox metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedVBox(@Nonnull VBox vbox,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the sidebar in the current traversal context.
	 *
	 * @param sidebar the sidebar metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitSidebar(@Nonnull Sidebar sidebar,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the sidebar after its child metadata.
	 *
	 * @param sidebar the sidebar metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedSidebar(@Nonnull Sidebar sidebar,
			boolean parentVisible,
			boolean parentEnabled);

	/**
	 * Visits the horizontal box in the current traversal context.
	 *
	 * @param hbox the hbox metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitHBox(@Nonnull HBox hbox,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Completes visitation of the horizontal box after its child metadata.
	 *
	 * @param hbox the hbox metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedHBox(@Nonnull HBox hbox,
										boolean parentVisible,
										boolean parentEnabled);

	// form
	
	/**
	 * Visits the form in the current traversal context.
	 *
	 * @param form the form metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitForm(@Nonnull Form form,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Completes visitation of the form after its child metadata.
	 *
	 * @param form the form metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedForm(@Nonnull Form form,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the form column in the current traversal context.
	 *
	 * @param column the column metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitFormColumn(@Nonnull FormColumn column,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the form row in the current traversal context.
	 *
	 * @param row the row metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitFormRow(@Nonnull FormRow row,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the form item in the current traversal context.
	 *
	 * @param item the item metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitFormItem(@Nonnull FormItem item,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the form item after its child metadata.
	 *
	 * @param item the item metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedFormItem(@Nonnull FormItem item,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the form row after its child metadata.
	 *
	 * @param row the row metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedFormRow(@Nonnull FormRow row,
											boolean parentVisible,
											boolean parentEnabled);

	// widgets
	
	/**
	 * Visits the button in the current traversal context.
	 *
	 * @param button the button metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitButton(@Nonnull Button button,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the zoom in in the current traversal context.
	 *
	 * @param zoomIn the zoom in metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitZoomIn(@Nonnull ZoomIn zoomIn,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the geometry in the current traversal context.
	 *
	 * @param geometry the geometry metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitGeometry(@Nonnull Geometry geometry,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the geometry after its child metadata.
	 *
	 * @param geometry the geometry metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedGeometry(@Nonnull Geometry geometry,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the geometry map in the current traversal context.
	 *
	 * @param geometry the geometry metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitGeometryMap(@Nonnull GeometryMap geometry,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the geometry map after its child metadata.
	 *
	 * @param geometry the geometry metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedGeometryMap(@Nonnull GeometryMap geometry,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the map in the current traversal context.
	 *
	 * @param map the map metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitMap(@Nonnull MapDisplay map,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Visits the chart in the current traversal context.
	 *
	 * @param chart the chart metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitChart(@Nonnull Chart chart,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the dialog button in the current traversal context.
	 *
	 * @param button the button metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDialogButton(@Nonnull DialogButton button,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the dynamic image in the current traversal context.
	 *
	 * @param image the image metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDynamicImage(@Nonnull DynamicImage image,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the spacer in the current traversal context.
	 *
	 * @param spacer the spacer metadata; must not be null
	 */
	public abstract void visitSpacer(@Nonnull Spacer spacer);

	/**
	 * Visits the static image in the current traversal context.
	 *
	 * @param image the image metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitStaticImage(@Nonnull StaticImage image,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the link in the current traversal context.
	 *
	 * @param link the link metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitLink(@Nonnull Link link,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Visits the blurb in the current traversal context.
	 *
	 * @param blurb the blurb metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitBlurb(@Nonnull Blurb blurb,
										boolean parentVisible,
										boolean parentEnabled);
	
	// bound widgets
	/**
	 * Visits the label in the current traversal context.
	 *
	 * @param label the label metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitLabel(@Nonnull Label label,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the progress bar in the current traversal context.
	 *
	 * @param progressBar the progress bar metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitProgressBar(@Nonnull ProgressBar progressBar,
											boolean parentVisible,
											boolean parentEnabled);

	// tabular widgets
	/**
	 * Visits the list grid in the current traversal context.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitListGrid(@Nonnull ListGrid grid,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the list grid after its child metadata.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedListGrid(@Nonnull ListGrid grid,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the list repeater in the current traversal context.
	 *
	 * @param repeater the repeater metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitListRepeater(@Nonnull ListRepeater repeater,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the list repeater after its child metadata.
	 *
	 * @param repeater the repeater metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedListRepeater(@Nonnull ListRepeater repeater,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the tree grid in the current traversal context.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitTreeGrid(@Nonnull TreeGrid grid,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the tree grid after its child metadata.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedTreeGrid(@Nonnull TreeGrid grid,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the data grid in the current traversal context.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDataGrid(@Nonnull DataGrid grid,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the data grid after its child metadata.
	 *
	 * @param grid the grid metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedDataGrid(@Nonnull DataGrid grid,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the data repeater in the current traversal context.
	 *
	 * @param repeater the repeater metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDataRepeater(@Nonnull DataRepeater repeater,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the data repeater after its child metadata.
	 *
	 * @param repeater the repeater metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedDataRepeater(@Nonnull DataRepeater repeater,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the data grid bound column in the current traversal context.
	 *
	 * @param column the column metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDataGridBoundColumn(@Nonnull DataGridBoundColumn column,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the data grid bound column after its child metadata.
	 *
	 * @param column the column metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedDataGridBoundColumn(@Nonnull DataGridBoundColumn column,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the data grid container column in the current traversal context.
	 *
	 * @param column the column metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitDataGridContainerColumn(@Nonnull DataGridContainerColumn column,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Completes visitation of the data grid container column after its child metadata.
	 *
	 * @param column the column metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedDataGridContainerColumn(@Nonnull DataGridContainerColumn column,
															boolean parentVisible,
															boolean parentEnabled);
	// input widgets
	
	/**
	 * Visits the check box in the current traversal context.
	 *
	 * @param checkBox the check box metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitCheckBox(@Nonnull CheckBox checkBox,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the check box after its child metadata.
	 *
	 * @param checkBox the check box metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedCheckBox(@Nonnull CheckBox checkBox,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the check membership in the current traversal context.
	 *
	 * @param membership the membership metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitCheckMembership(@Nonnull CheckMembership membership,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Completes visitation of the check membership after its child metadata.
	 *
	 * @param membership the membership metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedCheckMembership(@Nonnull CheckMembership membership,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Visits the colour picker in the current traversal context.
	 *
	 * @param colour the colour metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitColourPicker(@Nonnull ColourPicker colour,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the colour picker after its child metadata.
	 *
	 * @param colour the colour metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedColourPicker(@Nonnull ColourPicker colour,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the combo in the current traversal context.
	 *
	 * @param combo the combo metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitCombo(@Nonnull Combo combo,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the combo after its child metadata.
	 *
	 * @param combo the combo metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedCombo(@Nonnull Combo combo,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits a managed-content upload.
	 *
	 * @param content the content upload being visited; must not be {@code null}
	 * @param parentVisible whether ancestor metadata is visible
	 * @param parentEnabled whether ancestor metadata is enabled
	 */
	public abstract void visitContent(@Nonnull ContentUpload content,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the content signature in the current traversal context.
	 *
	 * @param signature the signature metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitContentSignature(@Nonnull ContentSignature signature,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the HTML widget in the current traversal context.
	 *
	 * @param html the HTML-widget metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitHTML(@Nonnull HTML html,
									boolean parentVisible,
									boolean parentEnabled);

	/**
	 * Visits the list membership in the current traversal context.
	 *
	 * @param membership the membership metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitListMembership(@Nonnull ListMembership membership,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Completes visitation of the list membership after its child metadata.
	 *
	 * @param membership the membership metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedListMembership(@Nonnull ListMembership membership,
												boolean parentVisible,
												boolean parentEnabled);

	/**
	 * Visits the comparison in the current traversal context.
	 *
	 * @param comparison the comparison metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitComparison(@Nonnull Comparison comparison,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the lookup description in the current traversal context.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitLookupDescription(@Nonnull LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the lookup description after its child metadata.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedLookupDescription(@Nonnull LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Visits the password in the current traversal context.
	 *
	 * @param password the password metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitPassword(@Nonnull Password password,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the password after its child metadata.
	 *
	 * @param password the password metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedPassword(@Nonnull Password password,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the radio in the current traversal context.
	 *
	 * @param radio the radio metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitRadio(@Nonnull Radio radio,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the radio after its child metadata.
	 *
	 * @param radio the radio metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedRadio(@Nonnull Radio radio,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the rich text in the current traversal context.
	 *
	 * @param richText the rich text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitRichText(@Nonnull RichText richText,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the rich text after its child metadata.
	 *
	 * @param richText the rich text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedRichText(@Nonnull RichText richText,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the slider in the current traversal context.
	 *
	 * @param slider the slider metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitSlider(@Nonnull Slider slider,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the slider after its child metadata.
	 *
	 * @param slider the slider metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedSlider(@Nonnull Slider slider,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the spinner in the current traversal context.
	 *
	 * @param spinner the spinner metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitSpinner(@Nonnull Spinner spinner,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the spinner after its child metadata.
	 *
	 * @param spinner the spinner metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedSpinner(@Nonnull Spinner spinner,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the text area in the current traversal context.
	 *
	 * @param text the text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitTextArea(@Nonnull TextArea text,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Completes visitation of the text area after its child metadata.
	 *
	 * @param text the text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedTextArea(@Nonnull TextArea text,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the text field in the current traversal context.
	 *
	 * @param text the text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitTextField(@Nonnull TextField text,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Completes visitation of the text field after its child metadata.
	 *
	 * @param text the text metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedTextField(@Nonnull TextField text,
											boolean parentVisible,
											boolean parentEnabled);

	/**
	 * Visits the inject in the current traversal context.
	 *
	 * @param inject the inject metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitInject(@Nonnull Inject inject,
										boolean parentVisible,
										boolean parentEnabled);

	/**
	 * Visits the on changed event handler before any associated child metadata.
	 *
	 * @param changeable the changeable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnChangedEventHandler(@Nonnull Changeable changeable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Completes visitation of the on changed event handler after its child metadata.
	 *
	 * @param changeable the changeable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnChangedEventHandler(@Nonnull Changeable changeable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on focus event handler before any associated child metadata.
	 *
	 * @param blurable the blurable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnFocusEventHandler(@Nonnull Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on focus event handler after its child metadata.
	 *
	 * @param blurable the blurable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnFocusEventHandler(@Nonnull Focusable blurable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on blur event handler before any associated child metadata.
	 *
	 * @param blurable the blurable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnBlurEventHandler(@Nonnull Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on blur event handler after its child metadata.
	 *
	 * @param blurable the blurable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnBlurEventHandler(@Nonnull Focusable blurable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Visits the on added event handler before any associated child metadata.
	 *
	 * @param addable the addable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnAddedEventHandler(@Nonnull Addable addable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on added event handler after its child metadata.
	 *
	 * @param addable the addable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnAddedEventHandler(@Nonnull Addable addable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on edited event handler before any associated child metadata.
	 *
	 * @param editable the editable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnEditedEventHandler(@Nonnull Editable editable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on edited event handler after its child metadata.
	 *
	 * @param editable the editable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnEditedEventHandler(@Nonnull Editable editable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on removed event handler before any associated child metadata.
	 *
	 * @param removable the removable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnRemovedEventHandler(@Nonnull Removable removable,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on removed event handler after its child metadata.
	 *
	 * @param removable the removable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnRemovedEventHandler(@Nonnull Removable removable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on selected event handler before any associated child metadata.
	 *
	 * @param selectable the selectable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnSelectedEventHandler(@Nonnull Selectable selectable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Completes visitation of the on selected event handler after its child metadata.
	 *
	 * @param selectable the selectable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnSelectedEventHandler(@Nonnull Selectable selectable,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on picked event handler before any associated child metadata.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnPickedEventHandler(@Nonnull LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on picked event handler after its child metadata.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnPickedEventHandler(@Nonnull LookupDescription lookup,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the on cleared event handler before any associated child metadata.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitOnClearedEventHandler(@Nonnull LookupDescription lookup,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Completes visitation of the on cleared event handler after its child metadata.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitedOnClearedEventHandler(@Nonnull LookupDescription lookup,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the rerender event action in the current traversal context.
	 *
	 * @param rerender the rerender metadata; must not be null
	 * @param source the metadata source that owns the event action; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitRerenderEventAction(@Nonnull RerenderEventAction rerender,
													@Nonnull EventSource source,
													boolean parentVisible,
													boolean parentEnabled);

	/**
	 * Visits the server side action event action in the current traversal context.
	 *
	 * @param server the server metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitServerSideActionEventAction(@Nonnull ServerSideActionEventAction server,
															boolean parentVisible,
															boolean parentEnabled);

	/**
	 * Visits the set disabled event action in the current traversal context.
	 *
	 * @param setDisabled the set disabled metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitSetDisabledEventAction(@Nonnull SetDisabledEventAction setDisabled,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the set invisible event action in the current traversal context.
	 *
	 * @param setInvisible the set invisible metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitSetInvisibleEventAction(@Nonnull SetInvisibleEventAction setInvisible,
														boolean parentVisible,
														boolean parentEnabled);

	/**
	 * Visits the toggle disabled event action in the current traversal context.
	 *
	 * @param toggleDisabled the toggle disabled metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitToggleDisabledEventAction(@Nonnull ToggleDisabledEventAction toggleDisabled,
															boolean parentVisible,
															boolean parentEnabled);

	/**
	 * Visits the toggle visibility event action in the current traversal context.
	 *
	 * @param toggleVisibility the toggle visibility metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public abstract void visitToggleVisibilityEventAction(@Nonnull ToggleVisibilityEventAction toggleVisibility,
															boolean parentVisible,
															boolean parentEnabled);

	/**
	 * Determines if an <code>Invisible</code> widget is visible or not.
	 * @param invisible	The widget to test.
	 * @return	if the widget is visible or not
	 */
	@SuppressWarnings("static-method")
	protected boolean visible(@Nonnull Invisible invisible) {
		return true;
	}
	
	/**
	 * Determines if a <code>Disableable</code> widget is visible or not.
	 * @param disableable	The widget to test.
	 * @return	if the widget is enabled or not
	 */
	@SuppressWarnings("static-method")
	protected boolean enabled(@Nonnull Disableable disableable) {
		return true;
	}

	/**
	 * Visits the widget in the current traversal context.
	 *
	 * @param widget the widget metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@SuppressWarnings({"java:S3776", "java:S6541"}) // complexity OK
	private void visitWidget(@Nonnull MetaData widget,
								boolean parentVisible,
								boolean parentEnabled) {
		// containers
		if (widget instanceof Container container) {
			visitContainer(container, parentVisible, parentEnabled);
		}
		else if (widget instanceof Form form) {
			visitForm(form, parentVisible, parentEnabled);
			boolean formVisible = parentVisible && visible(form);
			boolean formEnabled = parentEnabled && enabled(form);
			for (FormColumn column : form.getColumns()) {
				visitFormColumn(column, formVisible, formEnabled);
			}

			for (FormRow row : form.getRows()) {
				visitFormRow(row, formVisible, formEnabled);
				for (FormItem item : row.getItems()) {
					visitFormItem(item, formVisible, formEnabled);
					MetaData itemWidget = item.getWidget();
					if (itemWidget instanceof DefaultWidget defaultWidget) {
						visitDefaultWidget(defaultWidget, formVisible, formEnabled);
					}
					else {
						visitWidget(itemWidget, formVisible, formEnabled);
					}
					visitedFormItem(item, formVisible, formEnabled);
				}
				visitedFormRow(row, formVisible, formEnabled);
			}

			visitedForm(form, parentVisible, parentEnabled);
		}
		// widgets
		else if (widget instanceof TabPane tabPane) {
			visitTabPane(tabPane, parentVisible, parentEnabled);
			boolean tabPaneVisible = parentVisible && visible(tabPane);
			boolean tabPaneEnabled = parentEnabled && enabled(tabPane);
			for (Tab tab : tabPane.getTabs()) {
				visitContainer(tab, tabPaneVisible, tabPaneEnabled);
			}
			visitedTabPane(tabPane, parentVisible, parentEnabled);
		}
		else if (widget instanceof Button button) {
			visitButton(button, parentVisible, parentEnabled);
		}
		else if (widget instanceof ZoomIn zoomIn) {
			visitZoomIn(zoomIn, parentVisible, parentEnabled);
		}
		else if (widget instanceof Geometry geometry) {
			visitGeometry(geometry, parentVisible, parentEnabled);
			visitFocusable(geometry, parentVisible, parentEnabled);
			visitChangeable(geometry, parentVisible, parentEnabled);
		}
		else if (widget instanceof GeometryMap geometry) {
			visitGeometryMap(geometry, parentVisible, parentEnabled);
			visitChangeable(geometry, parentVisible, parentEnabled);
		}
		else if (widget instanceof MapDisplay map) {
			visitMap(map, parentVisible, parentEnabled);
		}
		else if (widget instanceof Chart chart) {
			visitChart(chart, parentVisible, parentEnabled);
		}
		else if (widget instanceof DialogButton button) {
			visitDialogButton(button, parentVisible, parentEnabled);
			visitParameterizable(button, parentVisible, parentEnabled);
		}
		else if (widget instanceof DynamicImage image) {
			visitDynamicImage(image, parentVisible, parentEnabled);
			visitParameterizable(image, parentVisible, parentEnabled);
		}
		else if (widget instanceof Spacer spacer) {
			visitSpacer(spacer);
		}
		else if (widget instanceof StaticImage image) {
			visitStaticImage(image, parentVisible, parentEnabled);
		}
		else if (widget instanceof Link link) {
			visitLink(link, parentVisible, parentEnabled);
		}
		else if (widget instanceof Blurb blurb) {
			visitBlurb(blurb, parentVisible, parentEnabled);
		}
		// bound
		else if (widget instanceof Label label) {
			visitLabel(label, parentVisible, parentEnabled);
		}
		else if (widget instanceof ProgressBar bar) {
			visitProgressBar(bar, parentVisible, parentEnabled);
		}
		// tabular
		else if (widget instanceof TreeGrid grid) {
			visitTreeGrid(grid, parentVisible, parentEnabled);
			visitFilterable(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedTreeGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof ListGrid grid) {
			visitListGrid(grid, parentVisible, parentEnabled);
			visitFilterable(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedListGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof ListRepeater repeater) {
			visitListRepeater(repeater, parentVisible, parentEnabled);
			visitFilterable(repeater, parentVisible, parentEnabled);
			visitedListRepeater(repeater, parentVisible, parentEnabled);
		}
		else if (widget instanceof DataGrid grid) {
			String gridBindingPrefix = grid.getBinding();
			if (gridBindingPrefix == null) {
				gridBindingPrefix = "";
			}
			else {
				gridBindingPrefix += '.';
			}
			visitDataGrid(grid, parentVisible, parentEnabled);
			boolean gridVisible = parentVisible && visible(grid);
			// Disregard grid.getEditable() as it could be that there are links that 
			// change the grid data client-side
			boolean gridEnabled = parentEnabled && enabled(grid);

			visitDataWidgetColumns(grid, gridBindingPrefix, gridVisible, gridEnabled, parentVisible, parentEnabled);

			visitAddableActions(grid, parentVisible, parentEnabled);
			visitEditableActions(grid, parentVisible, parentEnabled);
			visitRemovableActions(grid, parentVisible, parentEnabled);
			visitSelectableActions(grid, parentVisible, parentEnabled);
			visitedDataGrid(grid, parentVisible, parentEnabled);
		}
		else if (widget instanceof DataRepeater repeater) {
			String repeaterBindingPrefix = repeater.getBinding();
			if (repeaterBindingPrefix == null) {
				repeaterBindingPrefix = "";
			}
			else {
				repeaterBindingPrefix += '.';
			}
			visitDataRepeater(repeater, parentVisible, parentEnabled);
			boolean repeaterVisible = parentVisible && visible(repeater);

			visitDataWidgetColumns(repeater, repeaterBindingPrefix, repeaterVisible, true, parentVisible, parentEnabled);

			visitedDataRepeater(repeater, parentVisible, parentEnabled);
		}
		// input
		else if (widget instanceof CheckBox box) {
			visitCheckBox(box, parentVisible, parentEnabled);
			visitFocusable(box, parentVisible, parentEnabled);
			visitChangeable(box, parentVisible, parentEnabled);
			visitedCheckBox(box, parentVisible, parentEnabled);
		}
		else if (widget instanceof CheckMembership membership) {
			visitCheckMembership(membership, parentVisible, parentEnabled);
			visitFocusable(membership, parentVisible, parentEnabled);
			visitChangeable(membership, parentVisible, parentEnabled);
			visitedCheckMembership(membership, parentVisible, parentEnabled);
		}
		else if (widget instanceof ColourPicker colour) {
			visitColourPicker(colour, parentVisible, parentEnabled);
			visitFocusable(colour, parentVisible, parentEnabled);
			visitChangeable(colour, parentVisible, parentEnabled);
			visitedColourPicker(colour, parentVisible, parentEnabled);
		}
		else if (widget instanceof Combo combo) {
			visitCombo(combo, parentVisible, parentEnabled);
			visitFocusable(combo, parentVisible, parentEnabled);
			visitChangeable(combo, parentVisible, parentEnabled);
			visitedCombo(combo, parentVisible, parentEnabled);
		}
		else if (widget instanceof ContentUpload content) {
			visitContent(content, parentVisible, parentEnabled);
		}
		else if (widget instanceof ContentSignature signature) {
			visitContentSignature(signature, parentVisible, parentEnabled);
		}
		else if (widget instanceof HTML html) {
			visitHTML(html, parentVisible, parentEnabled);
		}
		else if (widget instanceof ListMembership membership) {
			visitListMembership(membership, parentVisible, parentEnabled);
			visitChangeable(membership, parentVisible, parentEnabled);
			visitedListMembership(membership, parentVisible, parentEnabled);
		}
		else if (widget instanceof Comparison comparison) {
			visitComparison(comparison, parentVisible, parentEnabled);
		}
		// subclass of Lookup, so test for it first
		else if (widget instanceof LookupDescription lookup) {
			visitLookupDescription(lookup, parentVisible, parentEnabled);
			visitLookupActions(lookup, parentVisible, parentEnabled);
			visitFilterable(lookup, parentVisible, parentEnabled);
			visitedLookupDescription(lookup, parentVisible, parentEnabled);
		}
		else if (widget instanceof Password password) {
			visitPassword(password, parentVisible, parentEnabled);
			visitFocusable(password, parentVisible, parentEnabled);
			visitChangeable(password, parentVisible, parentEnabled);
			visitedPassword(password, parentVisible, parentEnabled);
		}
		else if (widget instanceof Radio radio) {
			visitRadio(radio, parentVisible, parentEnabled);
			visitFocusable(radio, parentVisible, parentEnabled);
			visitChangeable(radio, parentVisible, parentEnabled);
			visitedRadio(radio, parentVisible, parentEnabled);
		}
		else if (widget instanceof RichText text) {
			visitRichText(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedRichText(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof Slider slider) {
			visitSlider(slider, parentVisible, parentEnabled);
			visitFocusable(slider, parentVisible, parentEnabled);
			visitChangeable(slider, parentVisible, parentEnabled);
			visitedSlider(slider, parentVisible, parentEnabled);
		}
		else if (widget instanceof Spinner spinner) {
			visitSpinner(spinner, parentVisible, parentEnabled);
			visitFocusable(spinner, parentVisible, parentEnabled);
			visitChangeable(spinner, parentVisible, parentEnabled);
			visitedSpinner(spinner, parentVisible, parentEnabled);
		}
		else if (widget instanceof TextArea text) {
			visitTextArea(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedTextArea(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof TextField text) {
			visitTextField(text, parentVisible, parentEnabled);
			visitFocusable(text, parentVisible, parentEnabled);
			visitChangeable(text, parentVisible, parentEnabled);
			visitedTextField(text, parentVisible, parentEnabled);
		}
		else if (widget instanceof Inject inject) {
			visitInject(inject, parentVisible, parentEnabled);
		}
		else if (widget instanceof Component component) {
			// Note that a view will resolve its components outside of this visitor as it may
			// require a different document and module etc if there is a binding
			visitComponent(component, parentVisible, parentEnabled);
		}
		else {
			throw new MetaDataException("Widget " + widget + " not catered for.");
		}
	}

	/**
	 * Visits the component in the current traversal context.
	 *
	 * @param component the component metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public void visitComponent(@Nonnull Component component,
								boolean parentVisible,
								boolean parentEnabled) {
		for (MetaData widget : component.getFragment(customer, currentUxUi).getContained()) {
			visitWidget(widget, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the default widget in the current traversal context.
	 *
	 * @param widget the widget metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	public void visitDefaultWidget(@Nonnull DefaultWidget widget, boolean parentVisible, boolean parentEnabled) {
		String binding = widget.getBinding();

		// determine the widget to use
		TargetMetaData target = BindUtil.getMetaDataForBinding(customer, module, document, binding);
		Attribute attribute = target.getAttribute();
		if (attribute != null) {
			Bound defaultWidget = attribute.getDefaultInputWidget();
			String definedBinding = defaultWidget.getBinding();
			try {
				// Temporarily set the binding in the default widget binding
				defaultWidget.setBinding(binding);
				visitWidget(defaultWidget, parentVisible, parentEnabled);
			}
			finally {
				defaultWidget.setBinding(definedBinding);
			}
		}
	}
	
	/**
	 * Visits the container in the current traversal context.
	 *
	 * @param container the container metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visitContainer(@Nonnull Container container,
									boolean parentVisible,
									boolean parentEnabled) {
		if (container == view) {
			visitView();
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, parentVisible, parentEnabled);
			}
			Sidebar sidebar = view.getSidebar();
			if (sidebar != null) {
				visitSidebar(sidebar, parentVisible, parentEnabled);
				boolean sidebarVisible = parentVisible && visible(sidebar);
				for (MetaData widget : sidebar.getContained()) {
					visitWidget(widget, sidebarVisible, parentEnabled);
				}
				visitedSidebar(sidebar, parentVisible, parentEnabled);
			}
			visitActions(view);
			visitedView();
		}
		else if (container instanceof Tab tab) {
			visitTab(tab, parentVisible, parentEnabled);
			boolean tabVisible = parentVisible && visible(tab);
			boolean tabEnabled = parentEnabled && enabled(tab);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, tabVisible, tabEnabled);
			}
			visitedTab(tab, parentVisible, parentEnabled);
		}
		else if (container instanceof VBox vbox) {
			visitVBox(vbox, parentVisible, parentEnabled);
			boolean vboxVisible = parentVisible && visible(vbox);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, vboxVisible, parentEnabled);
			}
			visitedVBox(vbox, parentVisible, parentEnabled);
		}
		else if (container instanceof HBox hbox) {
			visitHBox(hbox, parentVisible, parentEnabled);
			boolean hboxVisible = parentVisible && visible(hbox);
			for (MetaData widget : container.getContained()) {
				visitWidget(widget, hboxVisible, parentEnabled);
			}
			visitedHBox(hbox, parentVisible, parentEnabled);
		}
		else {
			throw new MetaDataException("Container " + container + " not catered for.");
		}
	}
	
	/**
	 * Visits the data widget columns in the current traversal context.
	 *
	 * @param widget the widget metadata; must not be null
	 * @param widgetBindingPrefix the compound-binding prefix for child columns; must not be null
	 * @param widgetVisible whether the data widget is visible
	 * @param widgetEnabled whether the data widget is enabled
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	@SuppressWarnings("java:S3776") // Complexity OK
	private void visitDataWidgetColumns(@Nonnull AbstractDataWidget widget,
											@Nonnull String widgetBindingPrefix,
											boolean widgetVisible,
											boolean widgetEnabled,
											boolean parentVisible,
											boolean parentEnabled) {
		for (TabularColumn column : widget.getColumns()) {
			if (column instanceof DataGridBoundColumn boundColumn) {
				visitDataGridBoundColumn(boundColumn, widgetVisible, widgetEnabled);

				boolean widgetColumnEnabled = widgetEnabled && (! Boolean.FALSE.equals(boundColumn.getEditable())); // can be true or null

				InputWidget inputWidget = null;
				String columnBinding = boundColumn.getBinding();
				WidgetReference widgetRef = boundColumn.getInputWidget();
				if (widgetRef != null) {
					inputWidget = widgetRef.getWidget();
				}
				else {
					// determine the widget to use
					String fullyQualifiedColumnBinding = columnBinding;
					if (fullyQualifiedColumnBinding == null) {
						fullyQualifiedColumnBinding = widget.getBinding();
					}
					else {
						fullyQualifiedColumnBinding = widgetBindingPrefix + fullyQualifiedColumnBinding;
					}
	
					if (fullyQualifiedColumnBinding.endsWith(Bean.BIZ_KEY)) {
						inputWidget = DocumentImpl.getBizKeyAttribute().getDefaultInputWidget();
					}
					else if (fullyQualifiedColumnBinding.endsWith(Bean.ORDINAL_NAME)) {
						inputWidget = DocumentImpl.getBizOrdinalAttribute().getDefaultInputWidget();
					}
					else if (useMetaData) {
						TargetMetaData target = BindUtil.getMetaDataForBinding(customer, 
																				module, 
																				document,
																				fullyQualifiedColumnBinding);
						Attribute attribute = target.getAttribute();
						if (attribute != null) {
							inputWidget = attribute.getDefaultInputWidget();
						}
					}
					else {
						inputWidget = DocumentImpl.getBizKeyAttribute().getDefaultInputWidget();
					}
				}
				
				if (inputWidget == null) {
					throw new MetaDataException("Could not determine the input widget to use from grid column " + widget.getBinding() + '.' + columnBinding);
				}

				String definedBinding = inputWidget.getBinding();
				try {
					// Temporarily set the binding to the datagrid column binding
					inputWidget.setBinding(columnBinding);
					visitWidget(inputWidget, widgetVisible, widgetColumnEnabled);
					visitedDataGridBoundColumn(boundColumn, widgetVisible, widgetEnabled);
				}
				finally {
					inputWidget.setBinding(definedBinding);
				}
			}
			else {
				DataGridContainerColumn containerColumn = (DataGridContainerColumn) column;
				visitDataGridContainerColumn(containerColumn, parentVisible, parentEnabled);
				
				for (MetaData containedWidget : containerColumn.getWidgets()) {
					visitWidget(containedWidget, parentVisible, parentEnabled);
				}
				
				visitedDataGridContainerColumn(containerColumn, parentVisible, parentEnabled);
			}
		}
	}
	
	/**
	 * Visits the changeable in the current traversal context.
	 *
	 * @param changeable the changeable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	protected void visitChangeable(@Nonnull Changeable changeable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<EventAction> actions = changeable.getChangedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnChangedEventHandler(changeable, parentVisible, parentEnabled);
			visitActions(changeable, actions, parentVisible, parentEnabled);
			visitedOnChangedEventHandler(changeable, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the focusable in the current traversal context.
	 *
	 * @param focusable the focusable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	protected void visitFocusable(@Nonnull Focusable focusable,
									boolean parentVisible,
									boolean parentEnabled) {
		List<EventAction> actions = focusable.getFocusActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnFocusEventHandler(focusable, parentVisible, parentEnabled);
			visitActions(focusable, actions, parentVisible, parentEnabled);
			visitedOnFocusEventHandler(focusable, parentVisible, parentEnabled);
		}
		actions = focusable.getBlurActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnBlurEventHandler(focusable, parentVisible, parentEnabled);
			visitActions(focusable, actions, parentVisible, parentEnabled);
			visitedOnBlurEventHandler(focusable, parentVisible, parentEnabled);
		}
	}
	
	/**
	 * Visits the lookup actions in the current traversal context.
	 *
	 * @param lookup the lookup metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitLookupActions(@Nonnull LookupDescription lookup,
										boolean parentVisible,
										boolean parentEnabled) {
		visitAddableActions(lookup, parentVisible, parentEnabled);
		visitEditableActions(lookup, parentVisible, parentEnabled);

		List<EventAction> actions = lookup.getPickedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnPickedEventHandler(lookup, parentVisible, parentEnabled);
			visitActions(lookup, actions, parentVisible, parentEnabled);
			visitedOnPickedEventHandler(lookup, parentVisible, parentEnabled);
		}
		actions = lookup.getClearedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnClearedEventHandler(lookup, parentVisible, parentEnabled);
			visitActions(lookup, actions, parentVisible, parentEnabled);
			visitedOnClearedEventHandler(lookup, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the addable actions in the current traversal context.
	 *
	 * @param addable the addable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitAddableActions(@Nonnull Addable addable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = addable.getAddedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnAddedEventHandler(addable, parentVisible, parentEnabled);
			visitActions(addable, actions, parentVisible, parentEnabled);
			visitedOnAddedEventHandler(addable, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the editable actions in the current traversal context.
	 *
	 * @param editable the editable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitEditableActions(@Nonnull Editable editable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = editable.getEditedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnEditedEventHandler(editable, parentVisible, parentEnabled);
			visitActions(editable, actions, parentVisible, parentEnabled);
			visitedOnEditedEventHandler(editable, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the removable actions in the current traversal context.
	 *
	 * @param removable the removable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitRemovableActions(@Nonnull Removable removable,
										boolean parentVisible,
										boolean parentEnabled) {
		List<EventAction> actions = removable.getRemovedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnRemovedEventHandler(removable, parentVisible, parentEnabled);
			visitActions(removable, actions, parentVisible, parentEnabled);
			visitedOnRemovedEventHandler(removable, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the selectable actions in the current traversal context.
	 *
	 * @param selectable the selectable metadata; must not be null
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitSelectableActions(@Nonnull Selectable selectable,
											boolean parentVisible,
											boolean parentEnabled) {
		List<EventAction> actions = selectable.getSelectedActions();
		if ((actions != null) && (! actions.isEmpty())) {
			visitOnSelectedEventHandler(selectable, parentVisible, parentEnabled);
			visitActions(selectable, actions, parentVisible, parentEnabled);
			visitedOnSelectedEventHandler(selectable, parentVisible, parentEnabled);
		}
	}

	/**
	 * Visits the actions in the current traversal context.
	 *
	 * @param source the metadata source that owns the event action; must not be null
	 * @param actions the ordered event actions, or null when none are declared
	 * @param parentVisible whether all ancestor metadata is visible
	 * @param parentEnabled whether all ancestor metadata is enabled
	 */
	private void visitActions(@Nonnull EventSource source,
								@Nullable List<EventAction> actions,
								boolean parentVisible, 
								boolean parentEnabled) {
		if (actions != null) {
			for (EventAction action : actions) {
				if (action instanceof RerenderEventAction rerender) {
					visitRerenderEventAction(rerender, source, parentVisible, parentEnabled);
				}
				else if (action instanceof ServerSideActionEventAction server) {
					visitServerSideActionEventAction(server, parentVisible, parentEnabled);
				}
				else if (action instanceof SetDisabledEventAction disabled) {
					visitSetDisabledEventAction(disabled, parentVisible, parentEnabled);
				}
				else if (action instanceof SetInvisibleEventAction invisible) {
					visitSetInvisibleEventAction(invisible, parentVisible, parentEnabled);
				}
				else if (action instanceof ToggleDisabledEventAction disabled) {
					visitToggleDisabledEventAction(disabled, parentVisible, parentEnabled);
				}
				else if (action instanceof ToggleVisibilityEventAction visibility) {
					visitToggleVisibilityEventAction(visibility, parentVisible, parentEnabled);
				}
				else {
					throw new MetaDataException(action + " is not catered for in ViewVisitor.visitChangeable()");
				}
			}
		}
	}
}
