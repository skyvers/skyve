package org.skyve.metadata.view.fluent;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.Container;
import org.skyve.impl.metadata.view.Identifiable;
import org.skyve.impl.metadata.view.Inject;
import org.skyve.impl.metadata.view.component.Component;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.TabPane;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
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
import org.skyve.impl.metadata.view.widget.bound.input.CheckMembership;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;
import org.skyve.metadata.MetaData;
import org.skyve.metadata.SerializableMetaData;

/**
 * Base fluent builder for metadata containers that can hold nested widgets.
 *
 * @param <T>
 *            the concrete fluent container type
 */
public abstract class FluentContainer<T extends FluentContainer<T>> extends FluentWidget {
	/**
	 * Initialises a fluent container builder.
	 */
	protected FluentContainer() {
		// nothing to see
	}
	
	/**
	 * Copies all contained widgets from the supplied container into this builder.
	 *
	 * <p>Side effects: appends converted widget metadata in source order to this container.
	 *
	 * @param container
	 *            the source container metadata
	 * @return this builder cast to the concrete fluent type
	 * @throws IllegalStateException
	 *             if a contained widget type is not supported by the fluent adapter set
	 */
	@SuppressWarnings("unchecked")
	protected T from(Container container) {
		for (SerializableMetaData widget : container.getContained()) {
			if (widget instanceof StaticImage image) {
				addStaticImage(new FluentStaticImage().from(image));
			}
			else if (widget instanceof VBox vbox) {
				addVBox(new FluentVBox().from(vbox));
			}
			else if (widget instanceof HBox hbox) {
				addHBox(new FluentHBox().from(hbox));
			}
			else if (widget instanceof Form form) {
				addForm(new FluentForm().from(form));
			}
			else if (widget instanceof TabPane pane) {
				addTabPane(new FluentTabPane().from(pane));
			}
			else if (widget instanceof Label label) {
				addLabel(new FluentLabel().from(label));
			}
			else if (widget instanceof Blurb blurb) {
				addBlurb(new FluentBlurb().from(blurb));
			}
			else if (widget instanceof Button button) {
				addButton(new FluentButton().from(button));
			}
			else if (widget instanceof DataGrid grid) {
				addDataGrid(new FluentDataGrid().from(grid));
			}
			else if (widget instanceof ListGrid grid) {
				addListGrid(new FluentListGrid().from(grid));
			}
			else if (widget instanceof Component component) {
				addComponent(new FluentComponent().from(component));
			}
			else if (widget instanceof ZoomIn zoom) {
				addZoomIn(new FluentZoomIn().from(zoom));
			}
			else if (widget instanceof Chart chart) {
				addChart(new FluentChart().from(chart));
			}
			else if (widget instanceof MapDisplay map) {
				addMapDisplay(new FluentMapDisplay().from(map));
			}
			else if (widget instanceof DynamicImage image) {
				addDynamicImage(new FluentDynamicImage().from(image));
			}
			else if (widget instanceof DialogButton button) {
				addDialogButton(new FluentDialogButton().from(button));
			}
			else if (widget instanceof Link link) {
				addLink(new FluentLink().from(link));
			}
			else if (widget instanceof Spacer spacer) {
				addSpacer(new FluentSpacer().from(spacer));
			}
			else if (widget instanceof ListMembership list) {
				addListMembership(new FluentListMembership().from(list));
			}
			else if (widget instanceof CheckMembership check) {
				addCheckMembership(new FluentCheckMembership().from(check));
			}
			else if (widget instanceof Comparison comparison) {
				addComparison(new FluentComparison().from(comparison));
			}
			else if (widget instanceof TreeGrid grid) {
				addTreeGrid(new FluentTreeGrid().from(grid));
			}
			else if (widget instanceof DataRepeater data) {
				addDataRepeater(new FluentDataRepeater().from(data));
			}
			else if (widget instanceof ListRepeater list) {
				addListRepeater(new FluentListRepeater().from(list));
			}
			else if (widget instanceof Inject inject) {
				addInject(new FluentInject().from(inject));
			}
			else {
				throw new IllegalStateException(widget + " is not catered for");
			}
		}
		return (T) this;
	}
	
	private <M extends MetaData> List<M> findWidgets(Class<M> widgetType) {
		List<M> result = new ArrayList<>();
		findWidgets(get().getContained(), widgetType, result);
		return result;
	}
	
	private static <M extends MetaData> void findWidgets(List<SerializableMetaData> contained, Class<M> widgetType, List<M> collect) {
		for (SerializableMetaData metadata : contained) {
			if (widgetType.equals(metadata.getClass())) {
				@SuppressWarnings("unchecked")
				M m = (M) metadata;
				collect.add(m);
			}
			if (metadata instanceof Container container) {
				findWidgets(container.getContained(), widgetType, collect);
			}
		}
	}

	private MetaData findIdentifiable(String widgetId) {
		return findIdentifiable(get().getContained(), widgetId);
	}

	private static SerializableMetaData findIdentifiable(List<SerializableMetaData> contained, String widgetId) {
		SerializableMetaData result = null;
		for (SerializableMetaData metadata : contained) {
			if ((metadata instanceof Identifiable identifiable) && widgetId.equals(identifiable.getWidgetId())) {
				result = metadata;
			}
			if ((result == null) && (metadata instanceof Container container)) {
				result = findIdentifiable(container.getContained(), widgetId);
			}
			if (result != null) {
				break;
			}
		}
		return result;
	}

	/**
	 * Appends a static image widget.
	 *
	 * @param image
	 *            the static image builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addStaticImage(FluentStaticImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}

	/**
	 * Inserts a static image widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param image
	 *            the static image builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addStaticImage(int index, FluentStaticImage image) {
		get().getContained().add(index, image.get());
		return (T) this;
	}

	/**
	 * Returns the static image at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the static image wrapper
	 */
	public FluentStaticImage getStaticImage(int index) {
		StaticImage result = (StaticImage) get().getContained().get(index);
		return new FluentStaticImage(result);
	}
	
	/**
	 * Finds all static images in this container tree.
	 *
	 * @return matching static image wrappers; never {@code null}
	 */
	public List<FluentStaticImage> findStaticImages() {
		List<StaticImage> list = findWidgets(StaticImage.class);
		List<FluentStaticImage> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentStaticImage(m)));
		return result;
	}
	
	/**
	 * Appends a vertical box container.
	 *
	 * @param vbox
	 *            the vertical box builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addVBox(FluentVBox vbox) {
		get().getContained().add(vbox.get());
		return (T) this;
	}

	/**
	 * Inserts a vertical box container at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param vbox
	 *            the vertical box builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addVBox(int index, FluentVBox vbox) {
		get().getContained().add(index, vbox.get());
		return (T) this;
	}

	/**
	 * Returns the vertical box at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the vertical box wrapper
	 */
	public FluentVBox getVBox(int index) {
		VBox result = (VBox) get().getContained().get(index);
		return new FluentVBox(result);
	}

	/**
	 * Finds all vertical boxes in this container tree.
	 *
	 * @return matching vertical box wrappers; never {@code null}
	 */
	public List<FluentVBox> findVBoxes() {
		List<VBox> list = findWidgets(VBox.class);
		List<FluentVBox> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentVBox(m)));
		return result;
	}

	/**
	 * Finds the first {@link VBox} with the supplied widget id in this container tree.
	 *
	 * <p>Complexity: O(n) where n is the total number of contained widgets, including nested containers.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentVBox} wrapper, or {@code null} if no match exists
	 */
	public FluentVBox findVBox(String widgetid) {
		VBox result = (VBox) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentVBox(result);
		}
		return null;
	}

	/**
	 * Appends a horizontal box container.
	 *
	 * @param hbox
	 *            the horizontal box builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addHBox(FluentHBox hbox) {
		get().getContained().add(hbox.get());
		return (T) this;
	}

	/**
	 * Inserts a horizontal box container at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param hbox
	 *            the horizontal box builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addHBox(int index, FluentHBox hbox) {
		get().getContained().add(index, hbox.get());
		return (T) this;
	}

	/**
	 * Returns the horizontal box at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the horizontal box wrapper
	 */
	public FluentHBox getHBox(int index) {
		HBox result = (HBox) get().getContained().get(index);
		return new FluentHBox(result);
	}

	/**
	 * Finds all horizontal boxes in this container tree.
	 *
	 * @return matching horizontal box wrappers; never {@code null}
	 */
	public List<FluentHBox> findHBoxes() {
		List<HBox> list = findWidgets(HBox.class);
		List<FluentHBox> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentHBox(m)));
		return result;
	}

	/**
	 * Finds the first {@link HBox} with the supplied widget id in this container tree.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentHBox} wrapper, or {@code null} if no match exists
	 */
	public FluentHBox findHBox(String widgetid) {
		HBox result = (HBox) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentHBox(result);
		}
		return null;
	}

	/**
	 * Appends a form container.
	 *
	 * @param form
	 *            the form builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addForm(FluentForm form) {
		get().getContained().add(form.get());
		return (T) this;
	}

	/**
	 * Inserts a form container at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param form
	 *            the form builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addForm(int index, FluentForm form) {
		get().getContained().add(index, form.get());
		return (T) this;
	}

	/**
	 * Returns the form at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the form wrapper
	 */
	public FluentForm getForm(int index) {
		Form result = (Form) get().getContained().get(index);
		return new FluentForm(result);
	}

	/**
	 * Finds all forms in this container tree.
	 *
	 * @return matching form wrappers; never {@code null}
	 */
	public List<FluentForm> findForms() {
		List<Form> list = findWidgets(Form.class);
		List<FluentForm> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentForm(m)));
		return result;
	}

	/**
	 * Finds the first {@link Form} with the supplied widget id in this container tree.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentForm} wrapper, or {@code null} if no match exists
	 */
	public FluentForm findForm(String widgetid) {
		Form result = (Form) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentForm(result);
		}
		return null;
	}
	
	/**
	 * Appends a tab pane container.
	 *
	 * @param tabPane
	 *            the tab pane builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addTabPane(FluentTabPane tabPane) {
		get().getContained().add(tabPane.get());
		return (T) this;
	}

	/**
	 * Inserts a tab pane container at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param tabPane
	 *            the tab pane builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addTabPane(int index, FluentTabPane tabPane) {
		get().getContained().add(index, tabPane.get());
		return (T) this;
	}

	/**
	 * Returns the tab pane at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the tab pane wrapper
	 */
	public FluentTabPane getTabPane(int index) {
		TabPane result = (TabPane) get().getContained().get(index);
		return new FluentTabPane(result);
	}

	/**
	 * Finds all tab panes in this container tree.
	 *
	 * @return matching tab pane wrappers; never {@code null}
	 */
	public List<FluentTabPane> findTabPanes() {
		List<TabPane> list = findWidgets(TabPane.class);
		List<FluentTabPane> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentTabPane(m)));
		return result;
	}

	/**
	 * Finds the first {@link TabPane} with the supplied widget id in this container tree.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentTabPane} wrapper, or {@code null} if no match exists
	 */
	public FluentTabPane findTabPane(String widgetid) {
		TabPane result = (TabPane) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentTabPane(result);
		}
		return null;
	}

	/**
	 * Appends a button widget.
	 *
	 * @param button
	 *            the button builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addButton(FluentButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}

	/**
	 * Inserts a button widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param button
	 *            the button builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addButton(int index, FluentButton button) {
		get().getContained().add(index, button.get());
		return (T) this;
	}

	/**
	 * Returns the button at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the button wrapper
	 */
	public FluentButton getButton(int index) {
		Button result = (Button) get().getContained().get(index);
		return new FluentButton(result);
	}

	/**
	 * Returns all button widgets contained in this container tree.
	 *
	 * @return matching button wrappers; never {@code null}
	 */
	public List<FluentButton> findButtons() {
		List<Button> list = findWidgets(Button.class);
		List<FluentButton> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentButton(m)));
		return result;
	}

	/**
	 * Appends a zoom-in widget.
	 *
	 * @param zoomIn
	 *            the zoom-in builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addZoomIn(FluentZoomIn zoomIn) {
		get().getContained().add(zoomIn.get());
		return (T) this;
	}

	/**
	 * Inserts a zoom-in widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param zoomIn
	 *            the zoom-in builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addZoomIn(int index, FluentZoomIn zoomIn) {
		get().getContained().add(index, zoomIn.get());
		return (T) this;
	}

	/**
	 * Returns the zoom-in widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the zoom-in wrapper
	 */
	public FluentZoomIn getZoomIn(int index) {
		ZoomIn result = (ZoomIn) get().getContained().get(index);
		return new FluentZoomIn(result);
	}

	/**
	 * Returns all zoom-in widgets contained in this container tree.
	 *
	 * @return matching zoom-in wrappers; never {@code null}
	 */
	public List<FluentZoomIn> findZoomIns() {
		List<ZoomIn> list = findWidgets(ZoomIn.class);
		List<FluentZoomIn> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentZoomIn(m)));
		return result;
	}

	/**
	 * Appends a chart widget.
	 *
	 * @param chart
	 *            the chart builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addChart(FluentChart chart) {
		get().getContained().add(chart.get());
		return (T) this;
	}
	
	/**
	 * Inserts a chart widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param chart
	 *            the chart builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addChart(int index, FluentChart chart) {
		get().getContained().add(index, chart.get());
		return (T) this;
	}

	/**
	 * Returns the chart at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the chart wrapper
	 */
	public FluentChart getChart(int index) {
		Chart result = (Chart) get().getContained().get(index);
		return new FluentChart(result);
	}

	/**
	 * Returns all chart widgets contained in this container tree.
	 *
	 * @return matching chart wrappers; never {@code null}
	 */
	public List<FluentChart> findCharts() {
		List<Chart> list = findWidgets(Chart.class);
		List<FluentChart> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentChart(m)));
		return result;
	}

	/**
	 * Appends a map display widget.
	 *
	 * @param map
	 *            the map display builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addMapDisplay(FluentMapDisplay map) {
		get().getContained().add(map.get());
		return (T) this;
	}

	/**
	 * Inserts a map display widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param map
	 *            the map display builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addMapDisplay(int index, FluentMapDisplay map) {
		get().getContained().add(index, map.get());
		return (T) this;
	}

	/**
	 * Returns the map display at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the map display wrapper
	 */
	public FluentMapDisplay getMapDisplay(int index) {
		MapDisplay result = (MapDisplay) get().getContained().get(index);
		return new FluentMapDisplay(result);
	}

	/**
	 * Returns all map display widgets contained in this container tree.
	 *
	 * @return matching map display wrappers; never {@code null}
	 */
	public List<FluentMapDisplay> findMapDisplays() {
		List<MapDisplay> list = findWidgets(MapDisplay.class);
		List<FluentMapDisplay> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentMapDisplay(m)));
		return result;
	}

	/**
	 * Appends a dynamic image widget.
	 *
	 * @param image
	 *            the dynamic image builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDynamicImage(FluentDynamicImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}

	/**
	 * Inserts a dynamic image widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param image
	 *            the dynamic image builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDynamicImage(int index, FluentDynamicImage image) {
		get().getContained().add(index, image.get());
		return (T) this;
	}

	/**
	 * Returns the dynamic image at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the dynamic image wrapper
	 */
	public FluentDynamicImage getDynamicImage(int index) {
		DynamicImage result = (DynamicImage) get().getContained().get(index);
		return new FluentDynamicImage(result);
	}

	/**
	 * Returns all dynamic image widgets contained in this container tree.
	 *
	 * @return matching dynamic image wrappers; never {@code null}
	 */
	public List<FluentDynamicImage> findDynamicImages() {
		List<DynamicImage> list = findWidgets(DynamicImage.class);
		List<FluentDynamicImage> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDynamicImage(m)));
		return result;
	}

	/**
	 * Appends a dialog button widget.
	 *
	 * @param button
	 *            the dialog button builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDialogButton(FluentDialogButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}

	/**
	 * Inserts a dialog button widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param button
	 *            the dialog button builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDialogButton(int index, FluentDialogButton button) {
		get().getContained().add(index, button.get());
		return (T) this;
	}

	/**
	 * Returns the dialog button at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the dialog button wrapper
	 */
	public FluentDialogButton getDialogButton(int index) {
		DialogButton result = (DialogButton) get().getContained().get(index);
		return new FluentDialogButton(result);
	}

	/**
	 * Returns all dialog button widgets contained in this container tree.
	 *
	 * @return matching dialog button wrappers; never {@code null}
	 */
	public List<FluentDialogButton> findDialogButtons() {
		List<DialogButton> list = findWidgets(DialogButton.class);
		List<FluentDialogButton> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDialogButton(m)));
		return result;
	}

	/**
	 * Appends a label widget.
	 *
	 * @param label
	 *            the label builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addLabel(FluentLabel label) {
		get().getContained().add(label.get());
		return (T) this;
	}

	/**
	 * Inserts a label widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param label
	 *            the label builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addLabel(int index, FluentLabel label) {
		get().getContained().add(index, label.get());
		return (T) this;
	}

	/**
	 * Returns the label at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the label wrapper
	 */
	public FluentLabel getLabel(int index) {
		Label result = (Label) get().getContained().get(index);
		return new FluentLabel(result);
	}

	/**
	 * Returns all label widgets contained in this container tree.
	 *
	 * @return matching label wrappers; never {@code null}
	 */
	public List<FluentLabel> findLabels() {
		List<Label> list = findWidgets(Label.class);
		List<FluentLabel> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentLabel(m)));
		return result;
	}

	/**
	 * Appends a blurb widget.
	 *
	 * @param blurb
	 *            the blurb builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addBlurb(FluentBlurb blurb) {
		get().getContained().add(blurb.get());
		return (T) this;
	}

	/**
	 * Inserts a blurb widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param blurb
	 *            the blurb builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addBlurb(int index, FluentBlurb blurb) {
		get().getContained().add(index, blurb.get());
		return (T) this;
	}

	/**
	 * Returns the blurb at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the blurb wrapper
	 */
	public FluentBlurb getBlurb(int index) {
		Blurb result = (Blurb) get().getContained().get(index);
		return new FluentBlurb(result);
	}

	/**
	 * Returns all blurb widgets contained in this container tree.
	 *
	 * @return matching blurb wrappers; never {@code null}
	 */
	public List<FluentBlurb> findBlurbs() {
		List<Blurb> list = findWidgets(Blurb.class);
		List<FluentBlurb> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentBlurb(m)));
		return result;
	}

	/**
	 * Appends a link widget.
	 *
	 * @param link
	 *            the link builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addLink(FluentLink link) {
		get().getContained().add(link.get());
		return (T) this;
	}

	/**
	 * Inserts a link widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param link
	 *            the link builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addLink(int index, FluentLink link) {
		get().getContained().add(index, link.get());
		return (T) this;
	}

	/**
	 * Returns the link at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the link wrapper
	 */
	public FluentLink getLink(int index) {
		Link result = (Link) get().getContained().get(index);
		return new FluentLink(result);
	}

	/**
	 * Returns all link widgets contained in this container tree.
	 *
	 * @return matching link wrappers; never {@code null}
	 */
	public List<FluentLink> findLinks() {
		List<Link> list = findWidgets(Link.class);
		List<FluentLink> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentLink(m)));
		return result;
	}

	/**
	 * Appends a spacer widget.
	 *
	 * @param spacer
	 *            the spacer builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addSpacer(FluentSpacer spacer) {
		get().getContained().add(spacer.get());
		return (T) this;
	}

	/**
	 * Inserts a spacer widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param spacer
	 *            the spacer builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addSpacer(int index, FluentSpacer spacer) {
		get().getContained().add(index, spacer.get());
		return (T) this;
	}

	/**
	 * Returns the spacer at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the spacer wrapper
	 */
	public FluentSpacer getSpacer(int index) {
		Spacer result = (Spacer) get().getContained().get(index);
		return new FluentSpacer(result);
	}

	/**
	 * Returns all spacer widgets contained in this container tree.
	 *
	 * @return matching spacer wrappers; never {@code null}
	 */
	public List<FluentSpacer> findSpacers() {
		List<Spacer> list = findWidgets(Spacer.class);
		List<FluentSpacer> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentSpacer(m)));
		return result;
	}

	/**
	 * Appends a list-membership widget.
	 *
	 * @param list
	 *            the list-membership builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListMembership(FluentListMembership list) {
		get().getContained().add(list.get());
		return (T) this;
	}

	/**
	 * Inserts a list-membership widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param list
	 *            the list-membership builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListMembership(int index, FluentListMembership list) {
		get().getContained().add(index, list.get());
		return (T) this;
	}

	/**
	 * Returns the list-membership widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the list-membership wrapper
	 */
	public FluentListMembership getListMembership(int index) {
		ListMembership result = (ListMembership) get().getContained().get(index);
		return new FluentListMembership(result);
	}

	/**
	 * Returns all list-membership widgets contained in this container tree.
	 *
	 * @return matching list-membership wrappers; never {@code null}
	 */
	public List<FluentListMembership> findListMemberships() {
		List<ListMembership> list = findWidgets(ListMembership.class);
		List<FluentListMembership> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListMembership(m)));
		return result;
	}

	/**
	 * Appends a check-membership widget.
	 *
	 * @param check
	 *            the check-membership builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addCheckMembership(FluentCheckMembership check) {
		get().getContained().add(check.get());
		return (T) this;
	}

	/**
	 * Inserts a check-membership widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param check
	 *            the check-membership builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addCheckMembership(int index, FluentCheckMembership check) {
		get().getContained().add(index, check.get());
		return (T) this;
	}

	/**
	 * Returns the check-membership widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the check-membership wrapper
	 */
	public FluentCheckMembership getCheckMembership(int index) {
		CheckMembership result = (CheckMembership) get().getContained().get(index);
		return new FluentCheckMembership(result);
	}

	/**
	 * Returns all check-membership widgets contained in this container tree.
	 *
	 * @return matching check-membership wrappers; never {@code null}
	 */
	public List<FluentCheckMembership> findCheckMemberships() {
		List<CheckMembership> list = findWidgets(CheckMembership.class);
		List<FluentCheckMembership> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentCheckMembership(m)));
		return result;
	}

	/**
	 * Appends a comparison widget.
	 *
	 * @param comparison
	 *            the comparison builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addComparison(FluentComparison comparison) {
		get().getContained().add(comparison.get());
		return (T) this;
	}

	/**
	 * Inserts a comparison widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param comparison
	 *            the comparison builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addComparison(int index, FluentComparison comparison) {
		get().getContained().add(index, comparison.get());
		return (T) this;
	}

	/**
	 * Returns the comparison widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the comparison wrapper
	 */
	public FluentComparison getComparison(int index) {
		Comparison result = (Comparison) get().getContained().get(index);
		return new FluentComparison(result);
	}

	/**
	 * Returns all comparison widgets contained in this container tree.
	 *
	 * @return matching comparison wrappers; never {@code null}
	 */
	public List<FluentComparison> findComparisons() {
		List<Comparison> list = findWidgets(Comparison.class);
		List<FluentComparison> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentComparison(m)));
		return result;
	}

	/**
	 * Appends a data-grid widget.
	 *
	 * @param grid
	 *            the data-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDataGrid(FluentDataGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}
	
	/**
	 * Inserts a data-grid widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param grid
	 *            the data-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDataGrid(int index, FluentDataGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	/**
	 * Returns the data grid at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the data-grid wrapper
	 */
	public FluentDataGrid getDataGrid(int index) {
		DataGrid result = (DataGrid) get().getContained().get(index);
		return new FluentDataGrid(result);
	}

	/**
	 * Returns all data-grid widgets contained in this container tree.
	 *
	 * @return matching data-grid wrappers; never {@code null}
	 */
	public List<FluentDataGrid> findDataGrids() {
		List<DataGrid> list = findWidgets(DataGrid.class);
		List<FluentDataGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDataGrid(m)));
		return result;
	}

	/**
	 * Finds the first {@link DataGrid} with the supplied widget id in this container tree.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentDataGrid} wrapper, or {@code null} if no match exists
	 */
	public FluentDataGrid findDataGrid(String widgetid) {
		DataGrid result = (DataGrid) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentDataGrid(result);
		}
		return null;
	}

	/**
	 * Appends a list-grid widget.
	 *
	 * @param grid
	 *            the list-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListGrid(FluentListGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	/**
	 * Inserts a list-grid widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param grid
	 *            the list-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListGrid(int index, FluentListGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	/**
	 * Returns the list grid at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the list-grid wrapper
	 */
	public FluentListGrid getListGrid(int index) {
		ListGrid result = (ListGrid) get().getContained().get(index);
		return new FluentListGrid(result);
	}

	/**
	 * Returns all list-grid widgets contained in this container tree.
	 *
	 * @return matching list-grid wrappers; never {@code null}
	 */
	public List<FluentListGrid> findListGrids() {
		List<ListGrid> list = findWidgets(ListGrid.class);
		List<FluentListGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListGrid(m)));
		return result;
	}

	/**
	 * Appends a tree-grid widget.
	 *
	 * @param grid
	 *            the tree-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addTreeGrid(FluentTreeGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	/**
	 * Inserts a tree-grid widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param grid
	 *            the tree-grid builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addTreeGrid(int index, FluentTreeGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	/**
	 * Returns the tree grid at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the tree-grid wrapper
	 */
	public FluentTreeGrid getTreeGrid(int index) {
		TreeGrid result = (TreeGrid) get().getContained().get(index);
		return new FluentTreeGrid(result);
	}

	/**
	 * Returns all tree-grid widgets contained in this container tree.
	 *
	 * @return matching tree-grid wrappers; never {@code null}
	 */
	public List<FluentTreeGrid> findTreeGrids() {
		List<TreeGrid> list = findWidgets(TreeGrid.class);
		List<FluentTreeGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentTreeGrid(m)));
		return result;
	}

	/**
	 * Appends a data-repeater widget.
	 *
	 * @param repeater
	 *            the data-repeater builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDataRepeater(FluentDataRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}

	/**
	 * Inserts a data-repeater widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param repeater
	 *            the data-repeater builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addDataRepeater(int index, FluentDataRepeater repeater) {
		get().getContained().add(index, repeater.get());
		return (T) this;
	}

	/**
	 * Returns the data repeater at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the data-repeater wrapper
	 */
	public FluentDataRepeater getDataRepeater(int index) {
		DataRepeater result = (DataRepeater) get().getContained().get(index);
		return new FluentDataRepeater(result);
	}

	/**
	 * Returns all data-repeater widgets contained in this container tree.
	 *
	 * @return matching data-repeater wrappers; never {@code null}
	 */
	public List<FluentDataRepeater> findDataRepeaters() {
		List<DataRepeater> list = findWidgets(DataRepeater.class);
		List<FluentDataRepeater> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDataRepeater(m)));
		return result;
	}

	/**
	 * Finds the first {@link DataRepeater} with the supplied widget id in this container tree.
	 *
	 * @param widgetid
	 *            the widget id to match
	 * @return the fluent {@link FluentDataRepeater} wrapper, or {@code null} if no match exists
	 */
	public FluentDataRepeater findDataRepeater(String widgetid) {
		DataRepeater result = (DataRepeater) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentDataRepeater(result);
		}
		return null;
	}

	/**
	 * Appends a list-repeater widget.
	 *
	 * @param repeater
	 *            the list-repeater builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListRepeater(FluentListRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}

	/**
	 * Inserts a list-repeater widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param repeater
	 *            the list-repeater builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addListRepeater(int index, FluentListRepeater repeater) {
		get().getContained().add(index, repeater.get());
		return (T) this;
	}

	/**
	 * Returns the list repeater at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the list-repeater wrapper
	 */
	public FluentListRepeater getListRepeater(int index) {
		ListRepeater result = (ListRepeater) get().getContained().get(index);
		return new FluentListRepeater(result);
	}

	/**
	 * Returns all list-repeater widgets contained in this container tree.
	 *
	 * @return matching list-repeater wrappers; never {@code null}
	 */
	public List<FluentListRepeater> findListRepeaters() {
		List<ListRepeater> list = findWidgets(ListRepeater.class);
		List<FluentListRepeater> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListRepeater(m)));
		return result;
	}

	/**
	 * Appends an inject widget.
	 *
	 * @param inject
	 *            the inject builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addInject(FluentInject inject) {
		get().getContained().add(inject.get());
		return (T) this;
	}

	/**
	 * Inserts an inject widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param inject
	 *            the inject builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addInject(int index, FluentInject inject) {
		get().getContained().add(index, inject.get());
		return (T) this;
	}

	/**
	 * Returns the inject widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the inject wrapper
	 */
	public FluentInject getInject(int index) {
		Inject result = (Inject) get().getContained().get(index);
		return new FluentInject(result);
	}

	/**
	 * Returns all inject widgets contained in this container tree.
	 *
	 * @return matching inject wrappers; never {@code null}
	 */
	public List<FluentInject> findInjects() {
		List<Inject> list = findWidgets(Inject.class);
		List<FluentInject> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentInject(m)));
		return result;
	}

	/**
	 * Appends a component widget.
	 *
	 * @param component
	 *            the component builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addComponent(FluentComponent component) {
		get().getContained().add(component.get());
		return (T) this;
	}

	/**
	 * Inserts a component widget at the supplied index.
	 *
	 * @param index
	 *            zero-based insertion index
	 * @param component
	 *            the component builder
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T addComponent(int index, FluentComponent component) {
		get().getContained().add(index, component.get());
		return (T) this;
	}

	/**
	 * Returns the component widget at the supplied position.
	 *
	 * @param index
	 *            zero-based index
	 * @return the component wrapper
	 */
	public FluentComponent getComponent(int index) {
		Component result = (Component) get().getContained().get(index);
		return new FluentComponent(result);
	}

	/**
	 * Returns all component widgets contained in this container tree.
	 *
	 * @return matching component wrappers; never {@code null}
	 */
	public List<FluentComponent> findComponents() {
		List<Component> list = findWidgets(Component.class);
		List<FluentComponent> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentComponent(m)));
		return result;
	}

	/**
	 * Removes the contained widget at the supplied index.
	 *
	 * @param index
	 *            zero-based index to remove
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T removeContained(int index) {
		get().getContained().remove(index);
		return (T) this;
	}
	
	/**
	 * Removes all directly contained widgets from this container.
	 *
	 * @return this builder
	 */
	@SuppressWarnings("unchecked")
	public T clearContained() {
		get().getContained().clear();
		return (T) this;
	}

	/**
	 * Returns the backing metadata instance.
	 *
	 * @return the mutable container metadata
	 */
	@Override
	public abstract Container get();
}
