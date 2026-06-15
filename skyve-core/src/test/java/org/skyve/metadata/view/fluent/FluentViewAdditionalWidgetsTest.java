package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.LoadingType;
import org.skyve.impl.metadata.view.container.Sidebar;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.model.chart.ChartBuilderMetaData;
import org.skyve.impl.metadata.view.widget.Chart;
import org.skyve.impl.metadata.view.widget.DialogButton;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.MapDisplay;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.input.Comparison;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.impl.metadata.view.widget.bound.input.TextArea;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridBoundColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGridContainerColumn;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataRepeater;

@SuppressWarnings("static-method")
class FluentViewAdditionalWidgetsTest {
	@Test
	void dialogButtonConstructorsSettersAndFromMapValues() {
		DialogButton source = new DialogButton();
		source.setDisplayName("Open Dialog");
		source.setDialogName("addressDialog");
		source.setCommand("openAddress");
		source.setDialogHeight(Integer.valueOf(420));
		source.setDialogWidth(Integer.valueOf(640));
		source.setModalDialog(Boolean.TRUE);
		source.setInvisibleConditionName("hideDialogButton");
		source.setDisabledConditionName("disableDialogButton");
		source.getParameters().add(new ParameterImpl());

		FluentDialogButton fluent = new FluentDialogButton().from(source);
		fluent.displayName("Display").dialogName("details").command("viewDetails").dialogHeight(320).dialogWidth(480).modalDialog(false);
		fluent.invisibleConditionName("hide").disabledConditionName("disable").addParameter(new FluentParameter().name("mode").value("edit"));

		assertThat(new FluentDialogButton().get(), is(notNullValue()));
		assertThat(fluent.get().getDisplayName(), is("Display"));
		assertThat(fluent.get().getDialogName(), is("details"));
		assertThat(fluent.get().getCommand(), is("viewDetails"));
		assertThat(fluent.get().getDialogHeight(), is(Integer.valueOf(320)));
		assertThat(fluent.get().getDialogWidth(), is(Integer.valueOf(480)));
		assertThat(fluent.get().isModalDialog(), is(Boolean.FALSE));
		assertEquals(2, fluent.get().getParameters().size());
	}

	@Test
	void mapDisplayAndDynamicImageFromAndSettersPopulateValues() {
		MapDisplay mapSource = new MapDisplay();
		mapSource.setModelName("mapModel");
		mapSource.setLoading(LoadingType.lazy);
		mapSource.setRefreshTimeInSeconds(Integer.valueOf(30));
		mapSource.setShowRefreshControls(Boolean.TRUE);
		mapSource.setInvisibleConditionName("hideMap");
		mapSource.setPixelWidth(Integer.valueOf(640));
		mapSource.setPixelHeight(Integer.valueOf(360));

		FluentMapDisplay map = new FluentMapDisplay().from(mapSource);
		map.modelName("updatedModel").loading(LoadingType.eager).refreshTimeInSeconds(45).showRefreshControls(false).invisibleConditionName("hideUpdated");
		map.pixelWidth(800).pixelHeight(400).percentageWidth(90).percentageHeight(60).minPixelWidth(250).maxPixelWidth(1000).minPixelHeight(150).maxPixelHeight(750);
		map.responsiveWidth(12).sm(3).md(5).lg(7).xl(9);
		assertThat(map.get().getModelName(), is("updatedModel"));
		assertThat(map.get().getLoading(), is(LoadingType.eager));
		assertThat(map.get().getRefreshTimeInSeconds(), is(Integer.valueOf(45)));
		assertThat(map.get().getShowRefreshControls(), is(Boolean.FALSE));

		DynamicImage imageSource = new DynamicImage();
		imageSource.setName("profilePhoto");
		imageSource.setImageInitialPixelWidth(Integer.valueOf(140));
		imageSource.setImageInitialPixelHeight(Integer.valueOf(120));
		imageSource.getParameters().add(new ParameterImpl());

		FluentDynamicImage image = new FluentDynamicImage().from(imageSource);
		image.name("updatedImage").imageInitialPixelWidth(160).imageInitialPixelHeight(130).pixelWidth(500).pixelHeight(320);
		image.minPixelWidth(220).maxPixelWidth(900).minPixelHeight(180).maxPixelHeight(640).percentageWidth(75).percentageHeight(55);
		image.responsiveWidth(11).sm(1).md(2).lg(3).xl(4).addParameter(new FluentParameter().name("size").value("large"));
		assertThat(new FluentDynamicImage().get(), is(notNullValue()));
		assertThat(image.get().getName(), is("updatedImage"));
		assertThat(image.get().getImageInitialPixelWidth(), is(Integer.valueOf(160)));
		assertThat(image.get().getImageInitialPixelHeight(), is(Integer.valueOf(130)));
		assertEquals(2, image.get().getParameters().size());
	}

	@Test
	void chartComparisonGeometryMapAndStaticImageFromAndSettersPopulateValues() {
		ChartBuilderMetaData model = new ChartBuilderMetaData();
		Chart chartSource = new Chart();
		chartSource.setType(Chart.ChartType.bar);
		chartSource.setModelName("salesModel");
		chartSource.setModel(model);
		chartSource.setInvisibleConditionName("hideChart");
		FluentChart chart = new FluentChart().from(chartSource);
		chart.type(Chart.ChartType.pie).modelName("updatedChartModel").model(model).invisibleConditionName("chartHidden");
		chart.pixelWidth(700).pixelHeight(320).minPixelWidth(200).maxPixelWidth(900).minPixelHeight(160).maxPixelHeight(600);
		chart.percentageWidth(85).percentageHeight(45).responsiveWidth(12).sm(2).md(4).lg(6).xl(8);
		assertThat(new FluentChart().get(), is(notNullValue()));
		assertThat(chart.get().getType(), is(Chart.ChartType.pie));

		Comparison comparisonSource = new Comparison();
		comparisonSource.setModelName("comparisonModel");
		comparisonSource.setEditable(Boolean.TRUE);
		comparisonSource.setBinding("bindingOne");
		comparisonSource.setDisabledConditionName("disableCmp");
		comparisonSource.setInvisibleConditionName("hideCmp");
		FluentComparison comparison = new FluentComparison().from(comparisonSource);
		comparison.modelName("updatedComparisonModel").editable(false).binding("bindingTwo");
		comparison.disabledConditionName("disableUpdated").invisibleConditionName("hideUpdated");
		comparison.pixelWidth(640).pixelHeight(460).minPixelWidth(280).maxPixelWidth(980).minPixelHeight(220).maxPixelHeight(700);
		comparison.percentageWidth(78).percentageHeight(66).responsiveWidth(9).sm(3).md(5).lg(7).xl(11);
		assertThat(new FluentComparison().get(), is(notNullValue()));
		assertThat(comparison.get().getModelName(), is("updatedComparisonModel"));
		assertThat(comparison.get().getEditable(), is(Boolean.FALSE));

		GeometryMap mapSource = new GeometryMap();
		mapSource.setType(GeometryInputType.polygon);
		mapSource.setBinding("geometryBinding");
		mapSource.setDisabledConditionName("disableGeometry");
		mapSource.setInvisibleConditionName("hideGeometry");
		mapSource.getChangedActions().add(new RerenderEventAction());
		FluentGeometryMap map = new FluentGeometryMap().from(mapSource);
		map.type(GeometryInputType.line).binding("updatedBinding").disabledConditionName("disableUpdated").invisibleConditionName("hideUpdated");
		map.pixelWidth(640).pixelHeight(320).minPixelWidth(300).maxPixelWidth(900).minPixelHeight(180).maxPixelHeight(640);
		map.percentageWidth(92).percentageHeight(58).responsiveWidth(12).sm(2).md(4).lg(6).xl(8);
		map.addAction(new FluentRerenderEventAction().clientValidation(false));
		assertThat(new FluentGeometryMap().get(), is(notNullValue()));
		assertThat(map.get().getType(), is(GeometryInputType.line));
		assertEquals(2, map.get().getChangedActions().size());

		StaticImage imageSource = new StaticImage();
		imageSource.setRelativeFile("images/source.svg");
		imageSource.setInvisibleConditionName("hideStatic");
		FluentStaticImage image = new FluentStaticImage().from(imageSource);
		image.relativeFile("images/updated.svg").invisibleConditionName("hideUpdated").pixelWidth(360).pixelHeight(220);
		image.minPixelWidth(120).maxPixelWidth(500).minPixelHeight(100).maxPixelHeight(300).percentageWidth(60).percentageHeight(40);
		image.responsiveWidth(8).sm(1).md(2).lg(3).xl(4);
		assertThat(new FluentStaticImage().get(), is(notNullValue()));
		assertThat(image.get().getRelativeFile(), is("images/updated.svg"));
		assertThat(image.get().getInvisibleConditionName(), is("hideUpdated"));
	}

	@Test
	void dataRepeaterTextAreaAndSidebarOperationsWork() {
		FluentDataRepeater repeater = new FluentDataRepeater();
		repeater.showColumnHeaders(Boolean.TRUE).showGrid(Boolean.FALSE).binding("rows").widgetId("repeaterWidget").title("Rows");
		repeater.pixelWidth(620).pixelHeight(360).minPixelWidth(220).maxPixelWidth(900).minPixelHeight(150).maxPixelHeight(700);
		repeater.percentageWidth(90).percentageHeight(55).responsiveWidth(10).sm(2).md(4).lg(6).xl(8).invisibleConditionName("hideRepeater");
		repeater.addBoundColumn(new FluentDataGridBoundColumn().binding("name").title("Name"));
		repeater.addContainerColumn(0, new FluentDataGridContainerColumn().title("Container"));
		assertThat(repeater.getContainerColumn(0).get().getTitle(), is("Container"));
		assertThat(repeater.getBoundColumn(1).get().getBinding(), is("name"));
		repeater.removeColumn(0);
		assertEquals(1, repeater.get().getColumns().size());
		repeater.clearColumns();
		assertEquals(0, repeater.get().getColumns().size());

		DataRepeater source = new DataRepeater();
		source.setShowColumnHeaders(Boolean.TRUE);
		source.setShowGrid(Boolean.TRUE);
		source.setBinding("sourceRows");
		DataGridBoundColumn bound = new DataGridBoundColumn();
		bound.setBinding("description");
		DataGridContainerColumn container = new DataGridContainerColumn();
		container.setTitle("WidgetContainer");
		source.getColumns().add(bound);
		source.getColumns().add(container);
		FluentDataRepeater fromSource = new FluentDataRepeater().from(source);
		assertThat(fromSource.get().getShowColumnHeaders(), is(Boolean.TRUE));
		assertThat(fromSource.get().getShowGrid(), is(Boolean.TRUE));
		assertEquals(2, fromSource.get().getColumns().size());

		TextArea textSource = new TextArea();
		textSource.setWordWrap(Boolean.TRUE);
		textSource.setEditable(Boolean.TRUE);
		textSource.setMinPixelHeight(Integer.valueOf(100));
		textSource.setKeyboardType(KeyboardType.numeric);
		textSource.setBinding("notes");
		textSource.setDisabledConditionName("disableNotes");
		textSource.setInvisibleConditionName("hideNotes");
		textSource.getChangedActions().add(new RerenderEventAction());
		textSource.getFocusActions().add(new RerenderEventAction());
		textSource.getBlurActions().add(new RerenderEventAction());
		FluentTextArea text = new FluentTextArea().from(textSource);
		text.wordWrap(false).editable(false).binding("updatedNotes").disabledConditionName("disableUpdated").invisibleConditionName("hideUpdated");
		assertThat(text.minPixelHeight(110), is(nullValue()));
		assertThat(text.keyboardType(KeyboardType.decimal), is(nullValue()));
		assertThat(text.pixelWidth(420), is(nullValue()));
		assertThat(text.pixelHeight(260), is(nullValue()));
		assertThat(text.get().getWordWrap(), is(Boolean.FALSE));
		assertThat(text.get().getEditable(), is(Boolean.FALSE));
		assertEquals(1, text.get().getChangedActions().size());
		assertEquals(1, text.get().getFocusActions().size());
		assertEquals(1, text.get().getBlurActions().size());

		Sidebar sidebarSource = new Sidebar();
		sidebarSource.setWidgetId("sourceSidebar");
		sidebarSource.setPixelWidth(Integer.valueOf(260));
		sidebarSource.setResponsiveWidth(Integer.valueOf(5));
		sidebarSource.setPercentageWidth(Integer.valueOf(40));
		sidebarSource.setFloatingPixelWidthBreakpoint(Integer.valueOf(1200));
		sidebarSource.setFloatingPixelWidth(Integer.valueOf(280));
		sidebarSource.setInvisibleConditionName("hideSidebar");
		sidebarSource.getContained().add(new StaticImage());
		FluentSidebar sidebar = new FluentSidebar().from(sidebarSource);
		sidebar.widgetId("updatedSidebar").pixelWidth(300).responsiveWidth(6).percentageWidth(50);
		sidebar.floatingPixelWidthBreakpoint(1300).floatingPixelWidth(320).invisibleConditionName("hideUpdated");
		sidebar.addStaticImage(new FluentStaticImage().relativeFile("sidebar.svg"));
		assertThat(new FluentSidebar().get(), is(notNullValue()));
		assertThat(sidebar.get().getWidgetId(), is("updatedSidebar"));
		assertThat(sidebar.get().getFloatingPixelWidthBreakpoint(), is(Integer.valueOf(1300)));
		assertThat(sidebar.get().getFloatingPixelWidth(), is(Integer.valueOf(320)));
		assertEquals(2, sidebar.get().getContained().size());
	}
}
