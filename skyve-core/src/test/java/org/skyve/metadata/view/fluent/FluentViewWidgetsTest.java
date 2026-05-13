package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryInputType;
import org.skyve.metadata.view.Action.ActionShow;

/** Unit tests for remaining fluent view widget builder classes. */
@SuppressWarnings("static-method")
class FluentViewWidgetsTest {

	// ---- FluentSlider ----

	@Test
	void sliderDefaultConstructorCreatesInstance() {
		FluentSlider s = new FluentSlider();
		assertThat(s.get(), is(notNullValue()));
	}

	@Test
	void sliderMinSetsValue() {
		FluentSlider s = new FluentSlider();
		s.min(0.5);
		assertThat(s.get().getMin(), is(0.5));
	}

	@Test
	void sliderMaxSetsValue() {
		FluentSlider s = new FluentSlider();
		s.max(100.0);
		assertThat(s.get().getMax(), is(100.0));
	}

	@Test
	void sliderNumberOfDiscreteValuesSetsValue() {
		FluentSlider s = new FluentSlider();
		s.numberOfDiscreteValues(5);
		assertThat(s.get().getNumberOfDiscreteValues(), is(5));
	}

	@Test
	void sliderRoundingPrecisionSetsValue() {
		FluentSlider s = new FluentSlider();
		s.roundingPrecision(2);
		assertThat(s.get().getRoundingPrecision(), is(2));
	}

	@Test
	void sliderVerticalSetsTrue() {
		FluentSlider s = new FluentSlider();
		s.vertical(true);
		assertThat(s.get().getVertical(), is(Boolean.TRUE));
	}

	@Test
	void sliderVerticalSetsFalse() {
		FluentSlider s = new FluentSlider();
		s.vertical(false);
		assertThat(s.get().getVertical(), is(Boolean.FALSE));
	}

	@Test
	void sliderPixelWidthSetsValue() {
		FluentSlider s = new FluentSlider();
		s.pixelWidth(300);
		assertThat(s.get().getPixelWidth(), is(300));
	}

	@Test
	void sliderPixelHeightSetsValue() {
		FluentSlider s = new FluentSlider();
		s.pixelHeight(50);
		assertThat(s.get().getPixelHeight(), is(50));
	}

	// ---- FluentSpinner ----

	@Test
	void spinnerDefaultConstructorCreatesInstance() {
		FluentSpinner s = new FluentSpinner();
		assertThat(s.get(), is(notNullValue()));
	}

	@Test
	void spinnerMinSetsValue() {
		FluentSpinner s = new FluentSpinner();
		s.min(1.0);
		assertThat(s.get().getMin(), is(1.0));
	}

	@Test
	void spinnerMaxSetsValue() {
		FluentSpinner s = new FluentSpinner();
		s.max(99.0);
		assertThat(s.get().getMax(), is(99.0));
	}

	@Test
	void spinnerStepSetsValue() {
		FluentSpinner s = new FluentSpinner();
		s.step(0.25);
		assertThat(s.get().getStep(), is(0.25));
	}

	// ---- FluentRadio ----

	@Test
	void radioDefaultConstructorCreatesInstance() {
		FluentRadio r = new FluentRadio();
		assertThat(r.get(), is(notNullValue()));
	}

	@Test
	void radioVerticalSetsTrue() {
		FluentRadio r = new FluentRadio();
		r.vertical(true);
		assertThat(r.get().getVertical(), is(Boolean.TRUE));
	}

	@Test
	void radioVerticalSetsFalse() {
		FluentRadio r = new FluentRadio();
		r.vertical(false);
		assertThat(r.get().getVertical(), is(Boolean.FALSE));
	}

	@Test
	void radioPixelWidthSetsValue() {
		FluentRadio r = new FluentRadio();
		r.pixelWidth(150);
		assertThat(r.get().getPixelWidth(), is(150));
	}

	// ---- FluentColourPicker ----

	@Test
	void colourPickerDefaultConstructorCreatesInstance() {
		FluentColourPicker c = new FluentColourPicker();
		assertThat(c.get(), is(notNullValue()));
	}

	@Test
	void colourPickerPixelWidthSetsValue() {
		FluentColourPicker c = new FluentColourPicker();
		c.pixelWidth(120);
		assertThat(c.get().getPixelWidth(), is(120));
	}

	// ---- FluentProgressBar ----

	@Test
	void progressBarDefaultConstructorCreatesInstance() {
		FluentProgressBar pb = new FluentProgressBar();
		assertThat(pb.get(), is(notNullValue()));
	}

	@Test
	void progressBarInvisibleConditionNameSetsValue() {
		FluentProgressBar pb = new FluentProgressBar();
		pb.invisibleConditionName("isHidden");
		assertThat(pb.get().getInvisibleConditionName(), is("isHidden"));
	}

	@Test
	void progressBarPixelWidthSetsValue() {
		FluentProgressBar pb = new FluentProgressBar();
		pb.pixelWidth(400);
		assertThat(pb.get().getPixelWidth(), is(400));
	}

	@Test
	void progressBarPixelHeightSetsValue() {
		FluentProgressBar pb = new FluentProgressBar();
		pb.pixelHeight(20);
		assertThat(pb.get().getPixelHeight(), is(20));
	}

	@Test
	void progressBarMinPixelWidthSetsValue() {
		FluentProgressBar pb = new FluentProgressBar();
		pb.minPixelWidth(100);
		assertThat(pb.get().getMinPixelWidth(), is(100));
	}

	@Test
	void progressBarMaxPixelWidthSetsValue() {
		FluentProgressBar pb = new FluentProgressBar();
		pb.maxPixelWidth(500);
		assertThat(pb.get().getMaxPixelWidth(), is(500));
	}

	// ---- FluentGeometry ----

	@Test
	void geometryDefaultConstructorCreatesInstance() {
		FluentGeometry g = new FluentGeometry();
		assertThat(g.get(), is(notNullValue()));
	}

	@Test
	void geometryTypeSetsPoint() {
		FluentGeometry g = new FluentGeometry();
		g.type(GeometryInputType.point);
		assertThat(g.get().getType(), is(GeometryInputType.point));
	}

	@Test
	void geometryTypeSetsPolygon() {
		FluentGeometry g = new FluentGeometry();
		g.type(GeometryInputType.polygon);
		assertThat(g.get().getType(), is(GeometryInputType.polygon));
	}

	@Test
	void geometryPixelWidthSetsValue() {
		FluentGeometry g = new FluentGeometry();
		g.pixelWidth(200);
		assertThat(g.get().getPixelWidth(), is(200));
	}

	// ---- FluentComponent ----

	@Test
	void componentDefaultConstructorCreatesInstance() {
		FluentComponent c = new FluentComponent();
		assertThat(c.get(), is(notNullValue()));
	}

	@Test
	void componentModuleNameSetsValue() {
		FluentComponent c = new FluentComponent();
		c.moduleName("admin");
		assertThat(c.get().getModuleName(), is("admin"));
	}

	@Test
	void componentDocumentNameSetsValue() {
		FluentComponent c = new FluentComponent();
		c.documentName("User");
		assertThat(c.get().getDocumentName(), is("User"));
	}

	@Test
	void componentNameSetsValue() {
		FluentComponent c = new FluentComponent();
		c.name("userPanel");
		assertThat(c.get().getName(), is("userPanel"));
	}

	@Test
	void componentWidgetIdSetsValue() {
		FluentComponent c = new FluentComponent();
		c.widgetId("w123");
		assertThat(c.get().getWidgetId(), is("w123"));
	}

	@Test
	void componentInvisibleConditionNameSetsValue() {
		FluentComponent c = new FluentComponent();
		c.invisibleConditionName("notAdmin");
		assertThat(c.get().getInvisibleConditionName(), is("notAdmin"));
	}

	// ---- FluentContentImage ----

	@Test
	void contentImageDefaultConstructorCreatesInstance() {
		FluentContentImage ci = new FluentContentImage();
		assertThat(ci.get(), is(notNullValue()));
	}

	@Test
	void contentImageEditableSetsTrue() {
		FluentContentImage ci = new FluentContentImage();
		ci.editable(true);
		assertThat(ci.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void contentImageEditableSetsFalse() {
		FluentContentImage ci = new FluentContentImage();
		ci.editable(false);
		assertThat(ci.get().getEditable(), is(Boolean.FALSE));
	}

	@Test
	void contentImageShowMarkupSetsTrue() {
		FluentContentImage ci = new FluentContentImage();
		ci.showMarkup(true);
		assertThat(ci.get().getShowMarkup(), is(Boolean.TRUE));
	}

	@Test
	void contentImagePixelWidthSetsValue() {
		FluentContentImage ci = new FluentContentImage();
		ci.pixelWidth(640);
		assertThat(ci.get().getPixelWidth(), is(640));
	}

	@Test
	void contentImagePixelHeightSetsValue() {
		FluentContentImage ci = new FluentContentImage();
		ci.pixelHeight(480);
		assertThat(ci.get().getPixelHeight(), is(480));
	}

	@Test
	void contentImagePercentageWidthSetsValue() {
		FluentContentImage ci = new FluentContentImage();
		ci.percentageWidth(50);
		assertThat(ci.get().getPercentageWidth(), is(50));
	}

	// ---- FluentContentLink ----

	@Test
	void contentLinkDefaultConstructorCreatesInstance() {
		FluentContentLink cl = new FluentContentLink();
		assertThat(cl.get(), is(notNullValue()));
	}

	@Test
	void contentLinkValueSetsValue() {
		FluentContentLink cl = new FluentContentLink();
		cl.value("http://example.com");
		assertThat(cl.get().getValue(), is("http://example.com"));
	}

	@Test
	void contentLinkEditableSetsTrue() {
		FluentContentLink cl = new FluentContentLink();
		cl.editable(true);
		assertThat(cl.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void contentLinkPixelWidthSetsValue() {
		FluentContentLink cl = new FluentContentLink();
		cl.pixelWidth(250);
		assertThat(cl.get().getPixelWidth(), is(250));
	}

	// ---- FluentZoomIn ----

	@Test
	void zoomInDefaultConstructorCreatesInstance() {
		FluentZoomIn z = new FluentZoomIn();
		assertThat(z.get(), is(notNullValue()));
	}

	@Test
	void zoomInDisplayNameSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.displayName("Open Record");
		assertThat(z.get().getDisplayName(), is("Open Record"));
	}

	@Test
	void zoomInIconStyleClassSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.iconStyleClass("fa fa-search");
		assertThat(z.get().getIconStyleClass(), is("fa fa-search"));
	}

	@Test
	void zoomInToolTipSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.toolTip("View details");
		assertThat(z.get().getToolTip(), is("View details"));
	}

	@Test
	void zoomInDisabledConditionNameSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.disabledConditionName("notEditable");
		assertThat(z.get().getDisabledConditionName(), is("notEditable"));
	}

	@Test
	void zoomInInvisibleConditionNameSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.invisibleConditionName("isHidden");
		assertThat(z.get().getInvisibleConditionName(), is("isHidden"));
	}

	@Test
	void zoomInShowSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.show(ActionShow.icon);
		assertThat(z.get().getShow(), is(ActionShow.icon));
	}

	@Test
	void zoomInPixelWidthSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.pixelWidth(100);
		assertThat(z.get().getPixelWidth(), is(100));
	}

	@Test
	void zoomInPixelHeightSetsValue() {
		FluentZoomIn z = new FluentZoomIn();
		z.pixelHeight(30);
		assertThat(z.get().getPixelHeight(), is(30));
	}

	// ---- FluentListRepeater ----

	@Test
	void listRepeaterDefaultConstructorCreatesInstance() {
		FluentListRepeater lr = new FluentListRepeater();
		assertThat(lr.get(), is(notNullValue()));
	}

	@Test
	void listRepeaterTitleSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.title("My List");
		assertThat(lr.get().getTitle(), is("My List"));
	}

	@Test
	void listRepeaterQueryNameSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.queryName("qContacts");
		assertThat(lr.get().getQueryName(), is("qContacts"));
	}

	@Test
	void listRepeaterModelNameSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.modelName("myModel");
		assertThat(lr.get().getModelName(), is("myModel"));
	}

	@Test
	void listRepeaterShowColumnHeadersSetsTrue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.showColumnHeaders(true);
		assertThat(lr.get().getShowColumnHeaders(), is(Boolean.TRUE));
	}

	@Test
	void listRepeaterShowGridSetsTrue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.showGrid(true);
		assertThat(lr.get().getShowGrid(), is(Boolean.TRUE));
	}

	@Test
	void listRepeaterInvisibleConditionNameSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.invisibleConditionName("notVisible");
		assertThat(lr.get().getInvisibleConditionName(), is("notVisible"));
	}

	@Test
	void listRepeaterPostRefreshConditionNameSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.postRefreshConditionName("afterRefresh");
		assertThat(lr.get().getPostRefreshConditionName(), is("afterRefresh"));
	}

	@Test
	void listRepeaterPixelWidthSetsValue() {
		FluentListRepeater lr = new FluentListRepeater();
		lr.pixelWidth(800);
		assertThat(lr.get().getPixelWidth(), is(800));
	}

	// ---- FluentTreeGrid ----

	@Test
	void treeGridDefaultConstructorCreatesInstance() {
		FluentTreeGrid tg = new FluentTreeGrid();
		assertThat(tg.get(), is(notNullValue()));
	}

	@Test
	void treeGridRootIdBindingSetsValue() {
		FluentTreeGrid tg = new FluentTreeGrid();
		tg.rootIdBinding("parentId");
		assertThat(tg.get().getRootIdBinding(), is("parentId"));
	}

	// ---- FluentLookupDescription ----

	@Test
	void lookupDescriptionDefaultConstructorCreatesInstance() {
		FluentLookupDescription ld = new FluentLookupDescription();
		assertThat(ld.get(), is(notNullValue()));
	}

	@Test
	void lookupDescriptionDescriptionBindingSetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.descriptionBinding("name");
		assertThat(ld.get().getDescriptionBinding(), is("name"));
	}

	@Test
	void lookupDescriptionQuerySetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.query("qContacts");
		assertThat(ld.get().getQuery(), is("qContacts"));
	}

	@Test
	void lookupDescriptionEditableSetsTrue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.editable(true);
		assertThat(ld.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void lookupDescriptionDisableEditConditionNameSetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.disableEditConditionName("notEditable");
		assertThat(ld.get().getDisableEditConditionName(), is("notEditable"));
	}

	@Test
	void lookupDescriptionDisableAddConditionNameSetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.disableAddConditionName("notAddable");
		assertThat(ld.get().getDisableAddConditionName(), is("notAddable"));
	}

	@Test
	void lookupDescriptionDisableClearConditionNameSetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.disableClearConditionName("notClearable");
		assertThat(ld.get().getDisableClearConditionName(), is("notClearable"));
	}

	@Test
	void lookupDescriptionDisablePickConditionNameSetsValue() {
		FluentLookupDescription ld = new FluentLookupDescription();
		ld.disablePickConditionName("notPickable");
		assertThat(ld.get().getDisablePickConditionName(), is("notPickable"));
	}

	// ---- FluentLookupDescriptionColumn ----

	@Test
	void lookupDescriptionColumnDefaultConstructorCreatesInstance() {
		FluentLookupDescriptionColumn c = new FluentLookupDescriptionColumn();
		assertThat(c.get(), is(notNullValue()));
	}

	@Test
	void lookupDescriptionColumnNameSetsValue() {
		FluentLookupDescriptionColumn c = new FluentLookupDescriptionColumn();
		c.name("firstName");
		assertThat(c.get().getName(), is("firstName"));
	}

	@Test
	void lookupDescriptionColumnFilterableSetsTrue() {
		FluentLookupDescriptionColumn c = new FluentLookupDescriptionColumn();
		c.filterable(true);
		assertThat(c.get().getFilterable(), is(Boolean.TRUE));
	}

	@Test
	void lookupDescriptionColumnFilterableSetsFalse() {
		FluentLookupDescriptionColumn c = new FluentLookupDescriptionColumn();
		c.filterable(false);
		assertThat(c.get().getFilterable(), is(Boolean.FALSE));
	}

	// ---- FluentDataGridBoundColumn ----

	@Test
	void dataGridBoundColumnDefaultConstructorCreatesInstance() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		assertThat(col.get(), is(notNullValue()));
	}

	@Test
	void dataGridBoundColumnBindingSetsValue() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		col.binding("name");
		assertThat(col.get().getBinding(), is("name"));
	}

	@Test
	void dataGridBoundColumnEditableSetsTrue() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		col.editable(true);
		assertThat(col.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void dataGridBoundColumnEscapeSetsTrue() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		col.escape(true);
		assertThat(col.get().getEscape(), is(Boolean.TRUE));
	}

	@Test
	void dataGridBoundColumnCustomFormatterSetsValue() {
		FluentDataGridBoundColumn col = new FluentDataGridBoundColumn();
		col.customFormatter("myFormatter");
		assertThat(col.get().getCustomFormatterName(), is("myFormatter"));
	}
}
