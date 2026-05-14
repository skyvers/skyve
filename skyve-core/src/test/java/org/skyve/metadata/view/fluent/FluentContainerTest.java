package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(images.size(), is(2));
	}

	@Test
	void addStaticImageAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addStaticImage(new FluentStaticImage());
		vbox.addStaticImage(0, new FluentStaticImage());
		assertThat(vbox.findStaticImages().size(), is(2));
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
		assertThat(vbox.findVBoxes().size(), is(2));
	}

	@Test
	void addVBoxAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addVBox(new FluentVBox());
		vbox.addVBox(0, new FluentVBox());
		assertThat(vbox.findVBoxes().size(), is(2));
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
		assertThat(vbox.findHBoxes().size(), is(1));
	}

	@Test
	void addHBoxAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addHBox(new FluentHBox());
		vbox.addHBox(0, new FluentHBox());
		assertThat(vbox.findHBoxes().size(), is(2));
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
		assertThat(vbox.findForms().size(), is(2));
	}

	@Test
	void addFormAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addForm(new FluentForm());
		vbox.addForm(0, new FluentForm());
		assertThat(vbox.findForms().size(), is(2));
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
		assertThat(vbox.findTabPanes().size(), is(2));
	}

	@Test
	void addTabPaneAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTabPane(new FluentTabPane());
		vbox.addTabPane(0, new FluentTabPane());
		assertThat(vbox.findTabPanes().size(), is(2));
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
		assertThat(vbox.findButtons().size(), is(2));
	}

	@Test
	void addButtonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addButton(0, new FluentButton());
		assertThat(vbox.findButtons().size(), is(2));
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
		assertThat(vbox.findZoomIns().size(), is(1));
	}

	@Test
	void addZoomInAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addZoomIn(new FluentZoomIn());
		vbox.addZoomIn(0, new FluentZoomIn());
		assertThat(vbox.findZoomIns().size(), is(2));
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
		assertThat(vbox.findCharts().size(), is(2));
	}

	@Test
	void addChartAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addChart(new FluentChart());
		vbox.addChart(0, new FluentChart());
		assertThat(vbox.findCharts().size(), is(2));
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
		assertThat(vbox.findMapDisplays().size(), is(1));
	}

	@Test
	void addMapDisplayAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addMapDisplay(new FluentMapDisplay());
		vbox.addMapDisplay(0, new FluentMapDisplay());
		assertThat(vbox.findMapDisplays().size(), is(2));
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
		assertThat(vbox.findDynamicImages().size(), is(2));
	}

	@Test
	void addDynamicImageAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDynamicImage(new FluentDynamicImage());
		vbox.addDynamicImage(0, new FluentDynamicImage());
		assertThat(vbox.findDynamicImages().size(), is(2));
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
		assertThat(vbox.findDialogButtons().size(), is(1));
	}

	@Test
	void addDialogButtonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDialogButton(new FluentDialogButton());
		vbox.addDialogButton(0, new FluentDialogButton());
		assertThat(vbox.findDialogButtons().size(), is(2));
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
		assertThat(vbox.findLabels().size(), is(2));
	}

	@Test
	void addLabelAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLabel(new FluentLabel());
		vbox.addLabel(0, new FluentLabel());
		assertThat(vbox.findLabels().size(), is(2));
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
		assertThat(vbox.findBlurbs().size(), is(1));
	}

	@Test
	void addBlurbAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addBlurb(new FluentBlurb());
		vbox.addBlurb(0, new FluentBlurb());
		assertThat(vbox.findBlurbs().size(), is(2));
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
		assertThat(vbox.findLinks().size(), is(2));
	}

	@Test
	void addLinkAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addLink(new FluentLink());
		vbox.addLink(0, new FluentLink());
		assertThat(vbox.findLinks().size(), is(2));
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
		assertThat(vbox.findSpacers().size(), is(1));
	}

	@Test
	void addSpacerAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addSpacer(new FluentSpacer());
		vbox.addSpacer(0, new FluentSpacer());
		assertThat(vbox.findSpacers().size(), is(2));
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
		assertThat(vbox.findListMemberships().size(), is(2));
	}

	@Test
	void addListMembershipAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListMembership(new FluentListMembership());
		vbox.addListMembership(0, new FluentListMembership());
		assertThat(vbox.findListMemberships().size(), is(2));
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
		assertThat(vbox.findCheckMemberships().size(), is(1));
	}

	@Test
	void addCheckMembershipAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addCheckMembership(new FluentCheckMembership());
		vbox.addCheckMembership(0, new FluentCheckMembership());
		assertThat(vbox.findCheckMemberships().size(), is(2));
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
		assertThat(vbox.findComparisons().size(), is(2));
	}

	@Test
	void addComparisonAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComparison(new FluentComparison());
		vbox.addComparison(0, new FluentComparison());
		assertThat(vbox.findComparisons().size(), is(2));
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
		assertThat(vbox.findDataGrids().size(), is(2));
	}

	@Test
	void addDataGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataGrid(new FluentDataGrid());
		vbox.addDataGrid(0, new FluentDataGrid());
		assertThat(vbox.findDataGrids().size(), is(2));
	}

	@Test
	void findDataGridByWidgetIdReturnsNullWhenNotFound() {
		FluentVBox vbox = new FluentVBox();
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
		assertThat(vbox.findListGrids().size(), is(2));
	}

	@Test
	void addListGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListGrid(new FluentListGrid());
		vbox.addListGrid(0, new FluentListGrid());
		assertThat(vbox.findListGrids().size(), is(2));
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
		assertThat(vbox.findTreeGrids().size(), is(1));
	}

	@Test
	void addTreeGridAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addTreeGrid(new FluentTreeGrid());
		vbox.addTreeGrid(0, new FluentTreeGrid());
		assertThat(vbox.findTreeGrids().size(), is(2));
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
		assertThat(vbox.findDataRepeaters().size(), is(2));
	}

	@Test
	void addDataRepeaterAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addDataRepeater(new FluentDataRepeater());
		vbox.addDataRepeater(0, new FluentDataRepeater());
		assertThat(vbox.findDataRepeaters().size(), is(2));
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
		assertThat(vbox.findListRepeaters().size(), is(2));
	}

	@Test
	void addListRepeaterAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addListRepeater(new FluentListRepeater());
		vbox.addListRepeater(0, new FluentListRepeater());
		assertThat(vbox.findListRepeaters().size(), is(2));
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
		assertThat(vbox.findInjects().size(), is(1));
	}

	@Test
	void addInjectAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addInject(new FluentInject());
		vbox.addInject(0, new FluentInject());
		assertThat(vbox.findInjects().size(), is(2));
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
		assertThat(vbox.findComponents().size(), is(2));
	}

	@Test
	void addComponentAtIndexInserts() {
		FluentVBox vbox = new FluentVBox();
		vbox.addComponent(new FluentComponent());
		vbox.addComponent(0, new FluentComponent());
		assertThat(vbox.findComponents().size(), is(2));
	}

	// ---- removeContained / clearContained ----

	@Test
	void removeContainedRemovesAtIndex() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addBlurb(new FluentBlurb());
		vbox.removeContained(0);
		assertThat(vbox.get().getContained().size(), is(1));
	}

	@Test
	void clearContainedRemovesAll() {
		FluentVBox vbox = new FluentVBox();
		vbox.addButton(new FluentButton());
		vbox.addBlurb(new FluentBlurb());
		vbox.addLabel(new FluentLabel());
		vbox.clearContained();
		assertThat(vbox.get().getContained().size(), is(0));
	}

	// ---- from() round-trip for widget types ----

	@Test
	void fromVBoxWithStaticImageRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new StaticImage());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findStaticImages().size(), is(1));
	}

	@Test
	void fromVBoxWithNestedVBoxRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new VBox());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findVBoxes().size(), is(1));
	}

	@Test
	void fromVBoxWithHBoxRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new HBox());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findHBoxes().size(), is(1));
	}

	@Test
	void fromVBoxWithFormRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Form());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findForms().size(), is(1));
	}

	@Test
	void fromVBoxWithTabPaneRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new TabPane());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findTabPanes().size(), is(1));
	}

	@Test
	void fromVBoxWithLabelRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Label());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findLabels().size(), is(1));
	}

	@Test
	void fromVBoxWithBlurbRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Blurb());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findBlurbs().size(), is(1));
	}

	@Test
	void fromVBoxWithButtonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Button());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findButtons().size(), is(1));
	}

	@Test
	void fromVBoxWithDataGridRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DataGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findDataGrids().size(), is(1));
	}

	@Test
	void fromVBoxWithListGridRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findListGrids().size(), is(1));
	}

	@Test
	void fromVBoxWithZoomInRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ZoomIn());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findZoomIns().size(), is(1));
	}

	@Test
	void fromVBoxWithChartRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Chart());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findCharts().size(), is(1));
	}

	@Test
	void fromVBoxWithMapDisplayRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new MapDisplay());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findMapDisplays().size(), is(1));
	}

	@Test
	void fromVBoxWithDynamicImageRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DynamicImage());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findDynamicImages().size(), is(1));
	}

	@Test
	void fromVBoxWithDialogButtonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DialogButton());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findDialogButtons().size(), is(1));
	}

	@Test
	void fromVBoxWithLinkRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Link());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findLinks().size(), is(1));
	}

	@Test
	void fromVBoxWithSpacerRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Spacer());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findSpacers().size(), is(1));
	}

	@Test
	void fromVBoxWithListMembershipRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListMembership());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findListMemberships().size(), is(1));
	}

	@Test
	void fromVBoxWithCheckMembershipRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new CheckMembership());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findCheckMemberships().size(), is(1));
	}

	@Test
	void fromVBoxWithComparisonRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new Comparison());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findComparisons().size(), is(1));
	}

	@Test
	void fromVBoxWithTreeGridTreatedAsListGrid() {
		// TreeGrid extends ListGrid, so the instanceof ListGrid branch in from() fires first;
		// the TreeGrid ends up stored as a plain ListGrid in the result.
		VBox source = new VBox();
		source.getContained().add(new TreeGrid());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findListGrids().size(), is(1));
		assertThat(fluent.findTreeGrids().size(), is(0));
	}

	@Test
	void fromVBoxWithDataRepeaterRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new DataRepeater());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findDataRepeaters().size(), is(1));
	}

	@Test
	void fromVBoxWithListRepeaterRoundTrips() {
		VBox source = new VBox();
		source.getContained().add(new ListRepeater());
		FluentVBox fluent = new FluentVBox().from(source);
		assertThat(fluent.findListRepeaters().size(), is(1));
	}

	// ---- findWidgets across nested containers ----

	@Test
	void findButtonsInNestedVBoxSearchesRecursively() {
		FluentVBox outer = new FluentVBox();
		FluentVBox inner = new FluentVBox();
		inner.addButton(new FluentButton());
		outer.addVBox(inner);
		// findWidgets is recursive — finds the button inside the nested VBox
		assertThat(outer.findButtons().size(), is(1));
		assertThat(inner.findButtons().size(), is(1));
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
}
