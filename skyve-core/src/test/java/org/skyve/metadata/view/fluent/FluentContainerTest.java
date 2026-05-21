package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;

import org.junit.jupiter.api.Test;
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

/**
 * Tests for {@link FluentContainer} widget management methods,
 * exercised via the concrete {@link FluentVBox} subclass.
 */
@SuppressWarnings("static-method")
class FluentContainerTest {

	// ---- Static image ----

	@Test
	void addStaticImageAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addStaticImage(new FluentStaticImage());
		assertThat(vbox.getStaticImage(0), is(notNullValue()));
		assertThat(vbox.getStaticImage(0).get(), instanceOf(StaticImage.class));
	}

	@Test
	void findStaticImagesReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addStaticImage(new FluentStaticImage());
		vbox.addStaticImage(new FluentStaticImage());
		List<FluentStaticImage> images = vbox.findStaticImages();
		assertEquals(2, images.size());
	}

	@Test
	void addStaticImageAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addStaticImage(new FluentStaticImage());
		vbox.addStaticImage(0, new FluentStaticImage());
		assertEquals(2, vbox.findStaticImages().size());
	}

	// ---- VBox ----

	@Test
	void addVBoxAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addVBox(new FluentVBox());
		assertThat(vbox.getVBox(0), is(notNullValue()));
		assertThat(vbox.getVBox(0).get(), instanceOf(VBox.class));
	}

	@Test
	void findVBoxesReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addVBox(new FluentVBox());
		vbox.addVBox(new FluentVBox());
		assertEquals(2, vbox.findVBoxes().size());
	}

	@Test
	void addVBoxAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addVBox(new FluentVBox());
		vbox.addVBox(0, new FluentVBox());
		assertEquals(2, vbox.findVBoxes().size());
	}

	@Test
	void findVBoxByWidgetIdReturnsMatch() {
		FluentVBox vbox = new FluentVBox();
		vbox.addVBox(new FluentVBox().widgetId("inner"));
		assertThat(vbox.findVBox("inner"), is(notNullValue()));
	}

	@Test
	void findVBoxByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findVBox("missing"), is(nullValue()));
	}

	// ---- HBox ----

	@Test
	void addHBoxAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addHBox(new FluentHBox());
		assertThat(vbox.getHBox(0), is(notNullValue()));
		assertThat(vbox.getHBox(0).get(), instanceOf(HBox.class));
	}

	@Test
	void findHBoxesReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addHBox(new FluentHBox());
		assertEquals(1, vbox.findHBoxes().size());
	}

	@Test
	void addHBoxAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addHBox(new FluentHBox());
		vbox.addHBox(0, new FluentHBox());
		assertEquals(2, vbox.findHBoxes().size());
	}

	@Test
	void findHBoxByWidgetIdReturnsMatch() {
		FluentVBox vbox = new FluentVBox();
		vbox.addHBox(new FluentHBox().widgetId("hboxId"));
		assertThat(vbox.findHBox("hboxId"), is(notNullValue()));
	}

	@Test
	void findHBoxByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findHBox("missing"), is(nullValue()));
	}

	// ---- Form ----

	@Test
	void addFormAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addForm(new FluentForm());
		assertThat(vbox.getForm(0), is(notNullValue()));
		assertThat(vbox.getForm(0).get(), instanceOf(Form.class));
	}

	@Test
	void findFormsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addForm(new FluentForm());
		vbox.addForm(new FluentForm());
		assertEquals(2, vbox.findForms().size());
	}

	@Test
	void addFormAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addForm(new FluentForm());
		vbox.addForm(0, new FluentForm());
		assertEquals(2, vbox.findForms().size());
	}

	@Test
	void findFormByWidgetIdReturnsMatch() {
		FluentVBox vbox = new FluentVBox();
		Form form = new Form();
		form.setWidgetId("formId");
		vbox.addForm(new FluentForm(form));
		assertThat(vbox.findForm("formId"), is(notNullValue()));
	}

	@Test
	void findFormByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findForm("missing"), is(nullValue()));
	}

	// ---- TabPane ----

	@Test
	void addTabPaneAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTabPane(new FluentTabPane());
		assertThat(vbox.getTabPane(0), is(notNullValue()));
		assertThat(vbox.getTabPane(0).get(), instanceOf(TabPane.class));
	}

	@Test
	void findTabPanesReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTabPane(new FluentTabPane());
		vbox.addTabPane(new FluentTabPane());
		assertEquals(2, vbox.findTabPanes().size());
	}

	@Test
	void addTabPaneAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTabPane(new FluentTabPane());
		vbox.addTabPane(0, new FluentTabPane());
		assertEquals(2, vbox.findTabPanes().size());
	}

	@Test
	void findTabPaneByWidgetIdReturnsMatch() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTabPane(new FluentTabPane().widgetId("paneId"));
		assertThat(vbox.findTabPane("paneId"), is(notNullValue()));
	}

	@Test
	void findTabPaneByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findTabPane("missing"), is(nullValue()));
	}

	// ---- Button ----

	@Test
	void addButtonAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		assertThat(vbox.getButton(0), is(notNullValue()));
		assertThat(vbox.getButton(0).get(), instanceOf(Button.class));
	}

	@Test
	void findButtonsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addButton(new FluentButton());
		assertEquals(2, vbox.findButtons().size());
	}

	@Test
	void addButtonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addButton(0, new FluentButton());
		assertEquals(2, vbox.findButtons().size());
	}

	// ---- ZoomIn ----

	@Test
	void addZoomInAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addZoomIn(new FluentZoomIn());
		assertThat(vbox.getZoomIn(0), is(notNullValue()));
		assertThat(vbox.getZoomIn(0).get(), instanceOf(ZoomIn.class));
	}

	@Test
	void findZoomInsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addZoomIn(new FluentZoomIn());
		assertEquals(1, vbox.findZoomIns().size());
	}

	@Test
	void addZoomInAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addZoomIn(new FluentZoomIn());
		vbox.addZoomIn(0, new FluentZoomIn());
		assertEquals(2, vbox.findZoomIns().size());
	}

	// ---- Chart ----

	@Test
	void addChartAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addChart(new FluentChart());
		assertThat(vbox.getChart(0), is(notNullValue()));
		assertThat(vbox.getChart(0).get(), instanceOf(Chart.class));
	}

	@Test
	void findChartsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addChart(new FluentChart());
		vbox.addChart(new FluentChart());
		assertEquals(2, vbox.findCharts().size());
	}

	@Test
	void addChartAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addChart(new FluentChart());
		vbox.addChart(0, new FluentChart());
		assertEquals(2, vbox.findCharts().size());
	}

	// ---- MapDisplay ----

	@Test
	void addMapDisplayAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addMapDisplay(new FluentMapDisplay());
		assertThat(vbox.getMapDisplay(0), is(notNullValue()));
		assertThat(vbox.getMapDisplay(0).get(), instanceOf(MapDisplay.class));
	}

	@Test
	void findMapDisplaysReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addMapDisplay(new FluentMapDisplay());
		assertEquals(1, vbox.findMapDisplays().size());
	}

	@Test
	void addMapDisplayAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addMapDisplay(new FluentMapDisplay());
		vbox.addMapDisplay(0, new FluentMapDisplay());
		assertEquals(2, vbox.findMapDisplays().size());
	}

	// ---- DynamicImage ----

	@Test
	void addDynamicImageAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDynamicImage(new FluentDynamicImage());
		assertThat(vbox.getDynamicImage(0), is(notNullValue()));
		assertThat(vbox.getDynamicImage(0).get(), instanceOf(DynamicImage.class));
	}

	@Test
	void findDynamicImagesReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDynamicImage(new FluentDynamicImage());
		vbox.addDynamicImage(new FluentDynamicImage());
		assertEquals(2, vbox.findDynamicImages().size());
	}

	@Test
	void addDynamicImageAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDynamicImage(new FluentDynamicImage());
		vbox.addDynamicImage(0, new FluentDynamicImage());
		assertEquals(2, vbox.findDynamicImages().size());
	}

	// ---- DialogButton ----

	@Test
	void addDialogButtonAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDialogButton(new FluentDialogButton());
		assertThat(vbox.getDialogButton(0), is(notNullValue()));
		assertThat(vbox.getDialogButton(0).get(), instanceOf(DialogButton.class));
	}

	@Test
	void findDialogButtonsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDialogButton(new FluentDialogButton());
		assertEquals(1, vbox.findDialogButtons().size());
	}

	@Test
	void addDialogButtonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDialogButton(new FluentDialogButton());
		vbox.addDialogButton(0, new FluentDialogButton());
		assertEquals(2, vbox.findDialogButtons().size());
	}

	// ---- Label ----

	@Test
	void addLabelAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLabel(new FluentLabel());
		assertThat(vbox.getLabel(0), is(notNullValue()));
		assertThat(vbox.getLabel(0).get(), instanceOf(Label.class));
	}

	@Test
	void findLabelsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLabel(new FluentLabel());
		vbox.addLabel(new FluentLabel());
		assertEquals(2, vbox.findLabels().size());
	}

	@Test
	void addLabelAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLabel(new FluentLabel());
		vbox.addLabel(0, new FluentLabel());
		assertEquals(2, vbox.findLabels().size());
	}

	// ---- Blurb ----

	@Test
	void addBlurbAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addBlurb(new FluentBlurb());
		assertThat(vbox.getBlurb(0), is(notNullValue()));
		assertThat(vbox.getBlurb(0).get(), instanceOf(Blurb.class));
	}

	@Test
	void findBlurbsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addBlurb(new FluentBlurb());
		assertEquals(1, vbox.findBlurbs().size());
	}

	@Test
	void addBlurbAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addBlurb(new FluentBlurb());
		vbox.addBlurb(0, new FluentBlurb());
		assertEquals(2, vbox.findBlurbs().size());
	}

	// ---- Link ----

	@Test
	void addLinkAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLink(new FluentLink());
		assertThat(vbox.getLink(0), is(notNullValue()));
		assertThat(vbox.getLink(0).get(), instanceOf(Link.class));
	}

	@Test
	void findLinksReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLink(new FluentLink());
		vbox.addLink(new FluentLink());
		assertEquals(2, vbox.findLinks().size());
	}

	@Test
	void addLinkAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLink(new FluentLink());
		vbox.addLink(0, new FluentLink());
		assertEquals(2, vbox.findLinks().size());
	}

	// ---- Spacer ----

	@Test
	void addSpacerAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addSpacer(new FluentSpacer());
		assertThat(vbox.getSpacer(0), is(notNullValue()));
		assertThat(vbox.getSpacer(0).get(), instanceOf(Spacer.class));
	}

	@Test
	void findSpacersReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addSpacer(new FluentSpacer());
		assertEquals(1, vbox.findSpacers().size());
	}

	@Test
	void addSpacerAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addSpacer(new FluentSpacer());
		vbox.addSpacer(0, new FluentSpacer());
		assertEquals(2, vbox.findSpacers().size());
	}

	// ---- ListMembership ----

	@Test
	void addListMembershipAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListMembership(new FluentListMembership());
		assertThat(vbox.getListMembership(0), is(notNullValue()));
		assertThat(vbox.getListMembership(0).get(), instanceOf(ListMembership.class));
	}

	@Test
	void findListMembershipsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListMembership(new FluentListMembership());
		vbox.addListMembership(new FluentListMembership());
		assertEquals(2, vbox.findListMemberships().size());
	}

	@Test
	void addListMembershipAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListMembership(new FluentListMembership());
		vbox.addListMembership(0, new FluentListMembership());
		assertEquals(2, vbox.findListMemberships().size());
	}

	// ---- CheckMembership ----

	@Test
	void addCheckMembershipAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addCheckMembership(new FluentCheckMembership());
		assertThat(vbox.getCheckMembership(0), is(notNullValue()));
		assertThat(vbox.getCheckMembership(0).get(), instanceOf(CheckMembership.class));
	}

	@Test
	void findCheckMembershipsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addCheckMembership(new FluentCheckMembership());
		assertEquals(1, vbox.findCheckMemberships().size());
	}

	@Test
	void addCheckMembershipAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addCheckMembership(new FluentCheckMembership());
		vbox.addCheckMembership(0, new FluentCheckMembership());
		assertEquals(2, vbox.findCheckMemberships().size());
	}

	// ---- Comparison ----

	@Test
	void addComparisonAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComparison(new FluentComparison());
		assertThat(vbox.getComparison(0), is(notNullValue()));
		assertThat(vbox.getComparison(0).get(), instanceOf(Comparison.class));
	}

	@Test
	void findComparisonsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComparison(new FluentComparison());
		vbox.addComparison(new FluentComparison());
		assertEquals(2, vbox.findComparisons().size());
	}

	@Test
	void addComparisonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComparison(new FluentComparison());
		vbox.addComparison(0, new FluentComparison());
		assertEquals(2, vbox.findComparisons().size());
	}

	// ---- DataGrid ----

	@Test
	void addDataGridAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataGrid(new FluentDataGrid());
		assertThat(vbox.getDataGrid(0), is(notNullValue()));
		assertThat(vbox.getDataGrid(0).get(), instanceOf(DataGrid.class));
	}

	@Test
	void findDataGridsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataGrid(new FluentDataGrid());
		vbox.addDataGrid(new FluentDataGrid());
		assertEquals(2, vbox.findDataGrids().size());
	}

	@Test
	void addDataGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataGrid(new FluentDataGrid());
		vbox.addDataGrid(0, new FluentDataGrid());
		assertEquals(2, vbox.findDataGrids().size());
	}

	@Test
	void findDataGridByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findDataGrid("missing"), is(nullValue()));
	}

	@Test
	void findDataGridByWidgetIdReturnsNullWhenContainedHasNonMatchingNonContainerWidget() {
		FluentVBox vbox = new FluentVBox();
		// Add a non-Container widget with a different widgetId so the for loop
		// iterates over it and exits normally (covering line 161 of findIdentifiable)
		vbox.addDataGrid(new FluentDataGrid().widgetId("other"));
		assertThat(vbox.findDataGrid("missing"), is(nullValue()));
	}

	// ---- ListGrid ----

	@Test
	void addListGridAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListGrid(new FluentListGrid());
		assertThat(vbox.getListGrid(0), is(notNullValue()));
		assertThat(vbox.getListGrid(0).get(), instanceOf(ListGrid.class));
	}

	@Test
	void findListGridsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListGrid(new FluentListGrid());
		vbox.addListGrid(new FluentListGrid());
		assertEquals(2, vbox.findListGrids().size());
	}

	@Test
	void addListGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListGrid(new FluentListGrid());
		vbox.addListGrid(0, new FluentListGrid());
		assertEquals(2, vbox.findListGrids().size());
	}

	// ---- TreeGrid ----

	@Test
	void addTreeGridAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTreeGrid(new FluentTreeGrid());
		assertThat(vbox.getTreeGrid(0), is(notNullValue()));
		assertThat(vbox.getTreeGrid(0).get(), instanceOf(TreeGrid.class));
	}

	@Test
	void findTreeGridsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTreeGrid(new FluentTreeGrid());
		assertEquals(1, vbox.findTreeGrids().size());
	}

	@Test
	void addTreeGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTreeGrid(new FluentTreeGrid());
		vbox.addTreeGrid(0, new FluentTreeGrid());
		assertEquals(2, vbox.findTreeGrids().size());
	}

	// ---- DataRepeater ----

	@Test
	void addDataRepeaterAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataRepeater(new FluentDataRepeater());
		assertThat(vbox.getDataRepeater(0), is(notNullValue()));
		assertThat(vbox.getDataRepeater(0).get(), instanceOf(DataRepeater.class));
	}

	@Test
	void findDataRepeatersReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataRepeater(new FluentDataRepeater());
		vbox.addDataRepeater(new FluentDataRepeater());
		assertEquals(2, vbox.findDataRepeaters().size());
	}

	@Test
	void addDataRepeaterAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataRepeater(new FluentDataRepeater());
		vbox.addDataRepeater(0, new FluentDataRepeater());
		assertEquals(2, vbox.findDataRepeaters().size());
	}

	@Test
	void findDataRepeaterByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
		assertThat(vbox.findDataRepeater("missing"), is(nullValue()));
	}

	// ---- ListRepeater ----

	@Test
	void addListRepeaterAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListRepeater(new FluentListRepeater());
		assertThat(vbox.getListRepeater(0), is(notNullValue()));
		assertThat(vbox.getListRepeater(0).get(), instanceOf(ListRepeater.class));
	}

	@Test
	void findListRepeatersReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListRepeater(new FluentListRepeater());
		vbox.addListRepeater(new FluentListRepeater());
		assertEquals(2, vbox.findListRepeaters().size());
	}

	@Test
	void addListRepeaterAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListRepeater(new FluentListRepeater());
		vbox.addListRepeater(0, new FluentListRepeater());
		assertEquals(2, vbox.findListRepeaters().size());
	}

	// ---- Inject ----

	@Test
	void addInjectAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addInject(new FluentInject());
		assertThat(vbox.getInject(0), is(notNullValue()));
	}

	@Test
	void findInjectsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addInject(new FluentInject());
		assertEquals(1, vbox.findInjects().size());
	}

	@Test
	void addInjectAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addInject(new FluentInject());
		vbox.addInject(0, new FluentInject());
		assertEquals(2, vbox.findInjects().size());
	}

	// ---- Component ----

	@Test
	void addComponentAndGetByIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComponent(new FluentComponent());
		assertThat(vbox.getComponent(0), is(notNullValue()));
	}

	@Test
	void findComponentsReturnsList() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComponent(new FluentComponent());
		vbox.addComponent(new FluentComponent());
		assertEquals(2, vbox.findComponents().size());
	}

	@Test
	void addComponentAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComponent(new FluentComponent());
		vbox.addComponent(0, new FluentComponent());
		assertEquals(2, vbox.findComponents().size());
	}

	// ---- removeContained / clearContained ----

	@Test
	void removeContainedRemovesAtIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addBlurb(new FluentBlurb());
		vbox.removeContained(0);
		assertEquals(1, vbox.get().getContained().size());
	}

	@Test
	void clearContainedRemovesAll() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addBlurb(new FluentBlurb());
		vbox.addLabel(new FluentLabel());
		vbox.clearContained();
		assertEquals(0, vbox.get().getContained().size());
	}

	// ---- from() round-trip for widget types ----

	@Test
	void fromVBoxWithStaticImageRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new StaticImage());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findStaticImages().size());
	}

	@Test
	void fromVBoxWithNestedVBoxRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new VBox());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findVBoxes().size());
	}

	@Test
	void fromVBoxWithHBoxRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new HBox());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findHBoxes().size());
	}

	@Test
	void fromVBoxWithFormRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Form());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findForms().size());
	}

	@Test
	void fromVBoxWithTabPaneRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new TabPane());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findTabPanes().size());
	}

	@Test
	void fromVBoxWithLabelRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Label());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findLabels().size());
	}

	@Test
	void fromVBoxWithBlurbRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Blurb());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findBlurbs().size());
	}

	@Test
	void fromVBoxWithButtonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Button());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findButtons().size());
	}

	@Test
	void fromVBoxWithDataGridRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DataGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findDataGrids().size());
	}

	@Test
	void fromVBoxWithListGridRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findListGrids().size());
	}

	@Test
	void fromVBoxWithZoomInRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ZoomIn());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findZoomIns().size());
	}

	@Test
	void fromVBoxWithChartRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Chart());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findCharts().size());
	}

	@Test
	void fromVBoxWithMapDisplayRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new MapDisplay());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findMapDisplays().size());
	}

	@Test
	void fromVBoxWithDynamicImageRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DynamicImage());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findDynamicImages().size());
	}

	@Test
	void fromVBoxWithDialogButtonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DialogButton());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findDialogButtons().size());
	}

	@Test
	void fromVBoxWithLinkRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Link());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findLinks().size());
	}

	@Test
	void fromVBoxWithSpacerRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Spacer());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findSpacers().size());
	}

	@Test
	void fromVBoxWithListMembershipRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListMembership());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findListMemberships().size());
	}

	@Test
	void fromVBoxWithCheckMembershipRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new CheckMembership());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findCheckMemberships().size());
	}

	@Test
	void fromVBoxWithComparisonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Comparison());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findComparisons().size());
	}

	@Test
	void fromVBoxWithTreeGridTreatedAsListGrid() {
		// TreeGrid extends ListGrid, so the instanceof ListGrid branch in from() fires first.
		// The TreeGrid ends up stored as a plain ListGrid in the result.
		VBox source = new VBox();
		source.getContained().add(new TreeGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findListGrids().size());
		assertEquals(0, fluent.findTreeGrids().size());
	}

	@Test
	void fromVBoxWithDataRepeaterRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DataRepeater());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findDataRepeaters().size());
	}

	@Test
	void fromVBoxWithListRepeaterRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListRepeater());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findListRepeaters().size());
	}

	// ---- findWidgets across nested containers ----

	@Test
	void findButtonsInNestedVBoxSearchesRecursively() {
		FluentVBox outer = new FluentVBox();
		FluentVBox inner = new FluentVBox();
		inner.addButton(new FluentButton());
		outer.addVBox(inner);
		// findWidgets is recursive — finds the button inside the nested VBox
		assertEquals(1, outer.findButtons().size());
		assertEquals(1, inner.findButtons().size());
	}

	@Test
	void findVBoxByWidgetIdSearchesNested() {
		FluentVBox outer = new FluentVBox();
		FluentVBox middle = new FluentVBox();
		FluentVBox inner = new FluentVBox().widgetId("deepId");
		middle.addVBox(inner);
		outer.addVBox(middle);
		assertThat(outer.findVBox("deepId"), is(notNullValue()));
	}

	@Test
	void fromVBoxWithComponentRoundTrips() {
		org.skyve.impl.metadata.view.container.VBox source = new org.skyve.impl.metadata.view.container.VBox();
		source.getContained().add(new org.skyve.impl.metadata.view.component.Component());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findComponents().size());
	}

	@Test
	void fromVBoxWithInjectRoundTrips() {
		org.skyve.impl.metadata.view.container.VBox source = new org.skyve.impl.metadata.view.container.VBox();
		source.getContained().add(new org.skyve.impl.metadata.view.Inject());
		FluentVBox fluent = new FluentVBox().from(source);
		assertEquals(1, fluent.findInjects().size());
	}

	@Test
	void fromVBoxWithUnknownWidgetThrowsIllegalState() {
		org.skyve.impl.metadata.view.container.VBox source = new org.skyve.impl.metadata.view.container.VBox();
		source.getContained().add(new org.skyve.impl.metadata.view.WidgetReference());
		FluentVBox fluentVBox = new FluentVBox();
		org.junit.jupiter.api.Assertions.assertThrows(IllegalStateException.class, () -> fluentVBox.from(source));
	}

	@Test
	void findDataGridByWidgetIdReturnsMatchedGrid() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataGrid(new FluentDataGrid().widgetId("wdg1"));
		assertThat(vbox.findDataGrid("wdg1"), is(notNullValue()));
	}

	@Test
	void findDataRepeaterByWidgetIdReturnsMatchedRepeater() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataRepeater(new FluentDataRepeater().widgetId("rep1"));
		assertThat(vbox.findDataRepeater("rep1"), is(notNullValue()));
	}

	@Test
	void fromWithTreeGridCoversTreeGridBranch() {
		VBox src = new VBox();
		src.getContained().add(new org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid());
		FluentVBox result = new FluentVBox().from(src);
		assertEquals(1, result.get().getContained().size());
	}
}
