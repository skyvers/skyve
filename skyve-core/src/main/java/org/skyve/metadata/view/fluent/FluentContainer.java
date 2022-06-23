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

abstract class FluentContainer<T extends FluentContainer<T>> extends FluentWidget {
	protected FluentContainer() {
		// nothing to see
	}
	
	@SuppressWarnings("unchecked")
	protected T from(Container container) {
		for (MetaData widget : container.getContained()) {
			if (widget instanceof StaticImage) {
				addStaticImage(new FluentStaticImage().from((StaticImage) widget));
			}
			else if (widget instanceof VBox) {
				addVBox(new FluentVBox().from((VBox) widget));
			}
			else if (widget instanceof HBox) {
				addHBox(new FluentHBox().from((HBox) widget));
			}
			else if (widget instanceof Form) {
				addForm(new FluentForm().from((Form) widget));
			}
			else if (widget instanceof TabPane) {
				addTabPane(new FluentTabPane().from((TabPane) widget));
			}
			else if (widget instanceof Label) {
				addLabel(new FluentLabel().from((Label) widget));
			}
			else if (widget instanceof Blurb) {
				addBlurb(new FluentBlurb().from((Blurb) widget));
			}
			else if (widget instanceof Button) {
				addButton(new FluentButton().from((Button) widget));
			}
			else if (widget instanceof DataGrid) {
				addDataGrid(new FluentDataGrid().from((DataGrid) widget));
			}
			else if (widget instanceof ListGrid) {
				addListGrid(new FluentListGrid().from((ListGrid) widget));
			}
			else if (widget instanceof Component) {
				addComponent(new FluentComponent().from((Component) widget));
			}
			else if (widget instanceof ZoomIn) {
				addZoomIn(new FluentZoomIn().from((ZoomIn) widget));
			}
			else if (widget instanceof Chart) {
				addChart(new FluentChart().from((Chart) widget));
			}
			else if (widget instanceof MapDisplay) {
				addMapDisplay(new FluentMapDisplay().from((MapDisplay) widget));
			}
			else if (widget instanceof DynamicImage) {
				addDynamicImage(new FluentDynamicImage().from((DynamicImage) widget));
			}
			else if (widget instanceof DialogButton) {
				addDialogButton(new FluentDialogButton().from((DialogButton) widget));
			}
			else if (widget instanceof Link) {
				addLink(new FluentLink().from((Link) widget));
			}
			else if (widget instanceof Spacer) {
				addSpacer(new FluentSpacer().from((Spacer) widget));
			}
			else if (widget instanceof ListMembership) {
				addListMembership(new FluentListMembership().from((ListMembership) widget));
			}
			else if (widget instanceof CheckMembership) {
				addCheckMembership(new FluentCheckMembership().from((CheckMembership) widget));
			}
			else if (widget instanceof Comparison) {
				addComparison(new FluentComparison().from((Comparison) widget));
			}
			else if (widget instanceof TreeGrid) {
				addTreeGrid(new FluentTreeGrid().from((TreeGrid) widget));
			}
			else if (widget instanceof DataRepeater) {
				addDataRepeater(new FluentDataRepeater().from((DataRepeater) widget));
			}
			else if (widget instanceof ListRepeater) {
				addListRepeater(new FluentListRepeater().from((ListRepeater) widget));
			}
			else if (widget instanceof Inject) {
				addInject(new FluentInject().from((Inject) widget));
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
	
	private static <M extends MetaData> void findWidgets(List<MetaData> contained, Class<M> widgetType, List<M> collect) {
		for (MetaData metadata : contained) {
			if (widgetType.equals(metadata.getClass())) {
				@SuppressWarnings("unchecked")
				M m = (M) metadata;
				collect.add(m);
			}
			if (metadata instanceof Container) {
				findWidgets(((Container) metadata).getContained(), widgetType, collect);
			}
		}
	}

	private MetaData findIdentifiable(String widgetId) {
		return findIdentifiable(get().getContained(), widgetId);
	}

	private static MetaData findIdentifiable(List<MetaData> contained, String widgetId) {
		MetaData result = null;
		for (MetaData metadata : contained) {
			if (metadata instanceof Identifiable) {
				if (widgetId.equals(((Identifiable) metadata).getWidgetId())) {
					result = metadata;
				}
			}
			if ((result == null) && (metadata instanceof Container)) {
				result = findIdentifiable(((Container) metadata).getContained(), widgetId);
			}
			if (result != null) {
				break;
			}
		}
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addStaticImage(FluentStaticImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addStaticImage(int index, FluentStaticImage image) {
		get().getContained().add(index, image.get());
		return (T) this;
	}

	public FluentStaticImage getStaticImage(int index) {
		StaticImage result = (StaticImage) get().getContained().get(index);
		return new FluentStaticImage(result);
	}
	
	public List<FluentStaticImage> findStaticImages() {
		List<StaticImage> list = findWidgets(StaticImage.class);
		List<FluentStaticImage> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentStaticImage(m)));
		return result;
	}
	
	@SuppressWarnings("unchecked")
	public T addVBox(FluentVBox vbox) {
		get().getContained().add(vbox.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addVBox(int index, FluentVBox vbox) {
		get().getContained().add(index, vbox.get());
		return (T) this;
	}

	public FluentVBox getVBox(int index) {
		VBox result = (VBox) get().getContained().get(index);
		return new FluentVBox(result);
	}

	public List<FluentVBox> findVBoxes() {
		List<VBox> list = findWidgets(VBox.class);
		List<FluentVBox> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentVBox(m)));
		return result;
	}

	public FluentVBox findVBox(String widgetid) {
		VBox result = (VBox) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentVBox(result);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public T addHBox(FluentHBox hbox) {
		get().getContained().add(hbox.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addHBox(int index, FluentHBox hbox) {
		get().getContained().add(index, hbox.get());
		return (T) this;
	}

	public FluentHBox getHBox(int index) {
		HBox result = (HBox) get().getContained().get(index);
		return new FluentHBox(result);
	}

	public List<FluentHBox> findHBoxes() {
		List<HBox> list = findWidgets(HBox.class);
		List<FluentHBox> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentHBox(m)));
		return result;
	}

	public FluentHBox findHBox(String widgetid) {
		HBox result = (HBox) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentHBox(result);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public T addForm(FluentForm form) {
		get().getContained().add(form.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addForm(int index, FluentForm form) {
		get().getContained().add(index, form.get());
		return (T) this;
	}

	public FluentForm getForm(int index) {
		Form result = (Form) get().getContained().get(index);
		return new FluentForm(result);
	}

	public List<FluentForm> findForms() {
		List<Form> list = findWidgets(Form.class);
		List<FluentForm> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentForm(m)));
		return result;
	}

	public FluentForm findForm(String widgetid) {
		Form result = (Form) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentForm(result);
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	public T addTabPane(FluentTabPane tabPane) {
		get().getContained().add(tabPane.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addTabPane(int index, FluentTabPane tabPane) {
		get().getContained().add(index, tabPane.get());
		return (T) this;
	}

	public FluentTabPane getTabPane(int index) {
		TabPane result = (TabPane) get().getContained().get(index);
		return new FluentTabPane(result);
	}

	public List<FluentTabPane> findTabPanes() {
		List<TabPane> list = findWidgets(TabPane.class);
		List<FluentTabPane> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentTabPane(m)));
		return result;
	}

	public FluentTabPane findTabPane(String widgetid) {
		TabPane result = (TabPane) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentTabPane(result);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public T addButton(FluentButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addButton(int index, FluentButton button) {
		get().getContained().add(index, button.get());
		return (T) this;
	}

	public FluentButton getButton(int index) {
		Button result = (Button) get().getContained().get(index);
		return new FluentButton(result);
	}

	public List<FluentButton> findButtons() {
		List<Button> list = findWidgets(Button.class);
		List<FluentButton> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentButton(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addZoomIn(FluentZoomIn zoomIn) {
		get().getContained().add(zoomIn.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addZoomIn(int index, FluentZoomIn zoomIn) {
		get().getContained().add(index, zoomIn.get());
		return (T) this;
	}

	public FluentZoomIn getZoomIn(int index) {
		ZoomIn result = (ZoomIn) get().getContained().get(index);
		return new FluentZoomIn(result);
	}

	public List<FluentZoomIn> findZoomIns() {
		List<ZoomIn> list = findWidgets(ZoomIn.class);
		List<FluentZoomIn> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentZoomIn(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addChart(FluentChart chart) {
		get().getContained().add(chart.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addChart(int index, FluentChart chart) {
		get().getContained().add(index, chart.get());
		return (T) this;
	}

	public FluentChart getChart(int index) {
		Chart result = (Chart) get().getContained().get(index);
		return new FluentChart(result);
	}

	public List<FluentChart> findCharts() {
		List<Chart> list = findWidgets(Chart.class);
		List<FluentChart> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentChart(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addMapDisplay(FluentMapDisplay map) {
		get().getContained().add(map.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addMapDisplay(int index, FluentMapDisplay map) {
		get().getContained().add(index, map.get());
		return (T) this;
	}

	public FluentMapDisplay getMapDisplay(int index) {
		MapDisplay result = (MapDisplay) get().getContained().get(index);
		return new FluentMapDisplay(result);
	}

	public List<FluentMapDisplay> findMapDisplays() {
		List<MapDisplay> list = findWidgets(MapDisplay.class);
		List<FluentMapDisplay> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentMapDisplay(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addDynamicImage(FluentDynamicImage image) {
		get().getContained().add(image.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDynamicImage(int index, FluentDynamicImage image) {
		get().getContained().add(index, image.get());
		return (T) this;
	}

	public FluentDynamicImage getDynamicImage(int index) {
		DynamicImage result = (DynamicImage) get().getContained().get(index);
		return new FluentDynamicImage(result);
	}

	public List<FluentDynamicImage> findDynamicImages() {
		List<DynamicImage> list = findWidgets(DynamicImage.class);
		List<FluentDynamicImage> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDynamicImage(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addDialogButton(FluentDialogButton button) {
		get().getContained().add(button.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDialogButton(int index, FluentDialogButton button) {
		get().getContained().add(index, button.get());
		return (T) this;
	}

	public FluentDialogButton getDialogButton(int index) {
		DialogButton result = (DialogButton) get().getContained().get(index);
		return new FluentDialogButton(result);
	}

	public List<FluentDialogButton> findDialogButtons() {
		List<DialogButton> list = findWidgets(DialogButton.class);
		List<FluentDialogButton> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDialogButton(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addLabel(FluentLabel label) {
		get().getContained().add(label.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addLabel(int index, FluentLabel label) {
		get().getContained().add(index, label.get());
		return (T) this;
	}

	public FluentLabel getLabel(int index) {
		Label result = (Label) get().getContained().get(index);
		return new FluentLabel(result);
	}

	public List<FluentLabel> findLabels() {
		List<Label> list = findWidgets(Label.class);
		List<FluentLabel> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentLabel(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addBlurb(FluentBlurb blurb) {
		get().getContained().add(blurb.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addBlurb(int index, FluentBlurb blurb) {
		get().getContained().add(index, blurb.get());
		return (T) this;
	}

	public FluentBlurb getBlurb(int index) {
		Blurb result = (Blurb) get().getContained().get(index);
		return new FluentBlurb(result);
	}

	public List<FluentBlurb> findBlurbs() {
		List<Blurb> list = findWidgets(Blurb.class);
		List<FluentBlurb> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentBlurb(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addLink(FluentLink link) {
		get().getContained().add(link.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addLink(int index, FluentLink link) {
		get().getContained().add(index, link.get());
		return (T) this;
	}

	public FluentLink getLink(int index) {
		Link result = (Link) get().getContained().get(index);
		return new FluentLink(result);
	}

	public List<FluentLink> findLinks() {
		List<Link> list = findWidgets(Link.class);
		List<FluentLink> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentLink(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addSpacer(FluentSpacer spacer) {
		get().getContained().add(spacer.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addSpacer(int index, FluentSpacer spacer) {
		get().getContained().add(index, spacer.get());
		return (T) this;
	}

	public FluentSpacer getSpacer(int index) {
		Spacer result = (Spacer) get().getContained().get(index);
		return new FluentSpacer(result);
	}

	public List<FluentSpacer> findSpacers() {
		List<Spacer> list = findWidgets(Spacer.class);
		List<FluentSpacer> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentSpacer(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addListMembership(FluentListMembership list) {
		get().getContained().add(list.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addListMembership(int index, FluentListMembership list) {
		get().getContained().add(index, list.get());
		return (T) this;
	}

	public FluentListMembership getListMembership(int index) {
		ListMembership result = (ListMembership) get().getContained().get(index);
		return new FluentListMembership(result);
	}

	public List<FluentListMembership> findListMemberships() {
		List<ListMembership> list = findWidgets(ListMembership.class);
		List<FluentListMembership> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListMembership(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addCheckMembership(FluentCheckMembership check) {
		get().getContained().add(check.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addCheckMembership(int index, FluentCheckMembership check) {
		get().getContained().add(index, check.get());
		return (T) this;
	}

	public FluentCheckMembership getCheckMembership(int index) {
		CheckMembership result = (CheckMembership) get().getContained().get(index);
		return new FluentCheckMembership(result);
	}

	public List<FluentCheckMembership> findCheckMemberships() {
		List<CheckMembership> list = findWidgets(CheckMembership.class);
		List<FluentCheckMembership> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentCheckMembership(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addComparison(FluentComparison comparison) {
		get().getContained().add(comparison.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addComparison(int index, FluentComparison comparison) {
		get().getContained().add(index, comparison.get());
		return (T) this;
	}

	public FluentComparison getComparison(int index) {
		Comparison result = (Comparison) get().getContained().get(index);
		return new FluentComparison(result);
	}

	public List<FluentComparison> findComparisons() {
		List<Comparison> list = findWidgets(Comparison.class);
		List<FluentComparison> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentComparison(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addDataGrid(FluentDataGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T addDataGrid(int index, FluentDataGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	public FluentDataGrid getDataGrid(int index) {
		DataGrid result = (DataGrid) get().getContained().get(index);
		return new FluentDataGrid(result);
	}

	public List<FluentDataGrid> findDataGrids() {
		List<DataGrid> list = findWidgets(DataGrid.class);
		List<FluentDataGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDataGrid(m)));
		return result;
	}

	public FluentDataGrid findDataGrid(String widgetid) {
		DataGrid result = (DataGrid) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentDataGrid(result);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public T addListGrid(FluentListGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addListGrid(int index, FluentListGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	public FluentListGrid getListGrid(int index) {
		ListGrid result = (ListGrid) get().getContained().get(index);
		return new FluentListGrid(result);
	}

	public List<FluentListGrid> findListGrids() {
		List<ListGrid> list = findWidgets(ListGrid.class);
		List<FluentListGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListGrid(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addTreeGrid(FluentTreeGrid grid) {
		get().getContained().add(grid.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addTreeGrid(int index, FluentTreeGrid grid) {
		get().getContained().add(index, grid.get());
		return (T) this;
	}

	public FluentTreeGrid getTreeGrid(int index) {
		TreeGrid result = (TreeGrid) get().getContained().get(index);
		return new FluentTreeGrid(result);
	}

	public List<FluentTreeGrid> findTreeGrids() {
		List<TreeGrid> list = findWidgets(TreeGrid.class);
		List<FluentTreeGrid> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentTreeGrid(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addDataRepeater(FluentDataRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addDataRepeater(int index, FluentDataRepeater repeater) {
		get().getContained().add(index, repeater.get());
		return (T) this;
	}

	public FluentDataRepeater getDataRepeater(int index) {
		DataRepeater result = (DataRepeater) get().getContained().get(index);
		return new FluentDataRepeater(result);
	}

	public List<FluentDataRepeater> findDataRepeaters() {
		List<DataRepeater> list = findWidgets(DataRepeater.class);
		List<FluentDataRepeater> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentDataRepeater(m)));
		return result;
	}

	public FluentDataRepeater findDataRepeater(String widgetid) {
		DataRepeater result = (DataRepeater) findIdentifiable(widgetid);
		if (result != null) {
			return new FluentDataRepeater(result);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public T addListRepeater(FluentListRepeater repeater) {
		get().getContained().add(repeater.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addListRepeater(int index, FluentListRepeater repeater) {
		get().getContained().add(index, repeater.get());
		return (T) this;
	}

	public FluentListRepeater getListRepeater(int index) {
		ListRepeater result = (ListRepeater) get().getContained().get(index);
		return new FluentListRepeater(result);
	}

	public List<FluentListRepeater> findListRepeaters() {
		List<ListRepeater> list = findWidgets(ListRepeater.class);
		List<FluentListRepeater> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentListRepeater(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addInject(FluentInject inject) {
		get().getContained().add(inject.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addInject(int index, FluentInject inject) {
		get().getContained().add(index, inject.get());
		return (T) this;
	}

	public FluentInject getInject(int index) {
		Inject result = (Inject) get().getContained().get(index);
		return new FluentInject(result);
	}

	public List<FluentInject> findInjects() {
		List<Inject> list = findWidgets(Inject.class);
		List<FluentInject> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentInject(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T addComponent(FluentComponent component) {
		get().getContained().add(component.get());
		return (T) this;
	}

	@SuppressWarnings("unchecked")
	public T addComponent(int index, FluentComponent component) {
		get().getContained().add(index, component.get());
		return (T) this;
	}

	public FluentComponent getComponent(int index) {
		Component result = (Component) get().getContained().get(index);
		return new FluentComponent(result);
	}

	public List<FluentComponent> findComponents() {
		List<Component> list = findWidgets(Component.class);
		List<FluentComponent> result = new ArrayList<>(list.size());
		list.forEach(m -> result.add(new FluentComponent(m)));
		return result;
	}

	@SuppressWarnings("unchecked")
	public T removeContained(int index) {
		get().getContained().remove(index);
		return (T) this;
	}
	
	@SuppressWarnings("unchecked")
	public T clearContained() {
		get().getContained().clear();
		return (T) this;
	}

	@Override
	public abstract Container get();
}
