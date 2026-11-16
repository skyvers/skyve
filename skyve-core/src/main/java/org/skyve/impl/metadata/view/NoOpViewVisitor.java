package org.skyve.impl.metadata.view;

import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
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
import org.skyve.metadata.view.widget.FilterParameter;
import org.skyve.metadata.view.widget.bound.Parameter;

import jakarta.annotation.Nonnull;

/**
 * No-operation adapter over {@link ViewVisitor} that overrides all abstract
 * visit methods with empty bodies.
 *
 * <p>Extend this class when only a subset of visit methods is needed, to
 * avoid being forced to implement the full {@link ViewVisitor} protocol.
 *
 * <p>Threading: not thread-safe; one instance per traversal.
 *
 * @see ViewVisitor
 */
public class NoOpViewVisitor extends ViewVisitor {
	/**
	 * Creates a no-op visitor bound to a specific traversal context.
	 *
	 * @param customer the active customer context
	 * @param module the active module context
	 * @param document the active document context
	 * @param view the view being traversed
	 * @param currentUxUi the active UX/UI profile
	 */
	protected NoOpViewVisitor(@Nonnull CustomerImpl customer,
								@Nonnull ModuleImpl module,
								@Nonnull DocumentImpl document,
								@Nonnull ViewImpl view,
								@Nonnull String currentUxUi) {
		super(customer, module, document, view, currentUxUi);
	}

	/** No-op. */
	@Override
	public void visitView() {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedView() {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedTabPane(@Nonnull TabPane tabPane, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedTab(@Nonnull Tab tab, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedVBox(@Nonnull VBox vbox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedHBox(@Nonnull HBox hbox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedForm(@Nonnull Form form, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitFormColumn(@Nonnull FormColumn column, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitFormRow(@Nonnull FormRow row, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitFormItem(@Nonnull FormItem item, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedFormItem(@Nonnull FormItem item, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedFormRow(@Nonnull FormRow row, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitButton(@Nonnull Button button, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitZoomIn(@Nonnull ZoomIn zoomIn, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitGeometry(@Nonnull Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedGeometry(@Nonnull Geometry geometry, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitGeometryMap(@Nonnull GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}
	
	/** No-op. */
	@Override
	public void visitedGeometryMap(@Nonnull GeometryMap geometry, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitMap(@Nonnull MapDisplay map, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitChart(@Nonnull Chart chart, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDialogButton(@Nonnull DialogButton button, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDynamicImage(@Nonnull DynamicImage image, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSpacer(@Nonnull Spacer spacer) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitStaticImage(@Nonnull StaticImage image, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitLink(@Nonnull Link link, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitBlurb(@Nonnull Blurb blurb, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitLabel(@Nonnull Label label, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitProgressBar(@Nonnull ProgressBar progressBar, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitListGrid(@Nonnull ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedListGrid(@Nonnull ListGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitListRepeater(@Nonnull ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedListRepeater(@Nonnull ListRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitTreeGrid(@Nonnull TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedTreeGrid(@Nonnull TreeGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedDataGrid(@Nonnull DataGrid grid, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedDataRepeater(@Nonnull DataRepeater repeater, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedDataGridBoundColumn(@Nonnull DataGridBoundColumn column, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDataGridContainerColumn(@Nonnull DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedDataGridContainerColumn(@Nonnull DataGridContainerColumn column, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitCheckBox(@Nonnull CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedCheckBox(@Nonnull CheckBox checkBox, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitCheckMembership(@Nonnull CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedCheckMembership(@Nonnull CheckMembership membership, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitColourPicker(@Nonnull ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedColourPicker(@Nonnull ColourPicker colour, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitCombo(@Nonnull Combo combo, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedCombo(@Nonnull Combo combo, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/**
	 * Ignores a managed-content upload.
	 *
	 * @param content the content upload being visited
	 * @param parentVisible whether ancestor metadata is visible
	 * @param parentEnabled whether ancestor metadata is enabled
	 */
	@Override
	public void visitContent(@Nonnull ContentUpload content, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitContentSignature(@Nonnull ContentSignature signature, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitHTML(@Nonnull HTML html, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitListMembership(@Nonnull ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedListMembership(@Nonnull ListMembership membership, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitComparison(@Nonnull Comparison comparison, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitLookupDescription(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedLookupDescription(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitPassword(@Nonnull Password password, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedPassword(@Nonnull Password password, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitRadio(@Nonnull Radio radio, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedRadio(@Nonnull Radio radio, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitRichText(@Nonnull RichText richText, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedRichText(@Nonnull RichText richText, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSlider(@Nonnull Slider slider, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedSlider(@Nonnull Slider slider, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSpinner(@Nonnull Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedSpinner(@Nonnull Spinner spinner, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitTextArea(@Nonnull TextArea text, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedTextArea(@Nonnull TextArea text, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitTextField(@Nonnull TextField text, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedTextField(@Nonnull TextField text, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitInject(@Nonnull Inject inject, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnChangedEventHandler(@Nonnull Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnChangedEventHandler(@Nonnull Changeable changeable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnFocusEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnFocusEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnBlurEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnBlurEventHandler(@Nonnull Focusable blurable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnAddedEventHandler(@Nonnull Addable addable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnAddedEventHandler(@Nonnull Addable addable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnEditedEventHandler(@Nonnull Editable editable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnEditedEventHandler(@Nonnull Editable editable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnRemovedEventHandler(@Nonnull Removable removable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnRemovedEventHandler(@Nonnull Removable removable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnSelectedEventHandler(@Nonnull Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnSelectedEventHandler(@Nonnull Selectable selectable, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnPickedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnPickedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOnClearedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedOnClearedEventHandler(@Nonnull LookupDescription lookup, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitRerenderEventAction(@Nonnull RerenderEventAction rerender, @Nonnull EventSource source, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitServerSideActionEventAction(@Nonnull ServerSideActionEventAction server, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSetDisabledEventAction(@Nonnull SetDisabledEventAction setDisabled, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSetInvisibleEventAction(@Nonnull SetInvisibleEventAction setInvisible, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitToggleDisabledEventAction(@Nonnull ToggleDisabledEventAction toggleDisabled, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitToggleVisibilityEventAction(@Nonnull ToggleVisibilityEventAction toggleVisibility, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitCustomAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitAddAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitRemoveAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitZoomOutAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitNavigateAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitOKAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSaveAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitCancelAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDeleteAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitReportAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitBizExportAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitBizImportAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitDownloadAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitUploadAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitNewAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitEditAction(@Nonnull ActionImpl action) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitParameter(@Nonnull Parameter parameter, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitFilterParameter(@Nonnull FilterParameter parameter, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}

	/** No-op. */
	@Override
	public void visitedSidebar(@Nonnull Sidebar sidebar, boolean parentVisible, boolean parentEnabled) {
		// no-op
	}
}
