package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.HorizontalAlignment;
import org.skyve.impl.metadata.view.ShrinkWrap;
import org.skyve.impl.metadata.view.VerticalAlignment;
import org.skyve.impl.metadata.view.container.Collapsible;
import org.skyve.impl.metadata.view.widget.bound.input.CompleteType;
import org.skyve.impl.metadata.view.widget.bound.input.KeyboardType;
import org.skyve.metadata.view.Action.ActionShow;
import org.skyve.metadata.view.TextOutput.Sanitisation;

/** Unit tests for fluent view builder classes. */
@SuppressWarnings("static-method")
class FluentViewBuildersTest {

	// ---- FluentTextField ----

	@Test
	void textFieldDefaultConstructorCreatesInstance() {
		FluentTextField f = new FluentTextField();
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void textFieldBindingSetsBinding() {
		FluentTextField f = new FluentTextField();
		f.binding("name");
		assertThat(f.get().getBinding(), is("name"));
	}

	@Test
	void textFieldEditableSetsTrue() {
		FluentTextField f = new FluentTextField();
		f.editable(true);
		assertThat(f.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void textFieldEditableSetsFalse() {
		FluentTextField f = new FluentTextField();
		f.editable(false);
		assertThat(f.get().getEditable(), is(Boolean.FALSE));
	}

	@Test
	void textFieldKeyboardTypeSetsValue() {
		FluentTextField f = new FluentTextField();
		f.keyboardType(KeyboardType.email);
		assertThat(f.get().getKeyboardType(), is(KeyboardType.email));
	}

	@Test
	void textFieldCompleteSetsValue() {
		FluentTextField f = new FluentTextField();
		f.complete(CompleteType.suggest);
		assertThat(f.get().getComplete(), is(CompleteType.suggest));
	}

	@Test
	void textFieldPixelWidthSetsValue() {
		FluentTextField f = new FluentTextField();
		f.pixelWidth(200);
		assertEquals(200, f.get().getPixelWidth());
	}

	@Test
	void textFieldInvisibleConditionNameSetsValue() {
		FluentTextField f = new FluentTextField();
		f.invisibleConditionName("hidden");
		assertThat(f.get().getInvisibleConditionName(), is("hidden"));
	}

	// ---- FluentCheckBox ----

	@Test
	void checkBoxDefaultConstructorCreatesInstance() {
		FluentCheckBox cb = new FluentCheckBox();
		assertThat(cb.get(), is(notNullValue()));
	}

	@Test
	void checkBoxBindingSetsBinding() {
		FluentCheckBox cb = new FluentCheckBox();
		cb.binding("active");
		assertThat(cb.get().getBinding(), is("active"));
	}

	@Test
	void checkBoxTriStateSetsTrue() {
		FluentCheckBox cb = new FluentCheckBox();
		cb.triState(true);
		assertThat(cb.get().getTriState(), is(Boolean.TRUE));
	}

	@Test
	void checkBoxTriStateSetsFalse() {
		FluentCheckBox cb = new FluentCheckBox();
		cb.triState(false);
		assertThat(cb.get().getTriState(), is(Boolean.FALSE));
	}

	@Test
	void checkBoxPixelWidthSetsValue() {
		FluentCheckBox cb = new FluentCheckBox();
		cb.pixelWidth(50);
		assertEquals(50, cb.get().getPixelWidth());
	}

	@Test
	void checkBoxPixelHeightSetsValue() {
		FluentCheckBox cb = new FluentCheckBox();
		cb.pixelHeight(30);
		assertEquals(30, cb.get().getPixelHeight());
	}

	// ---- FluentCombo ----

	@Test
	void comboDefaultConstructorCreatesInstance() {
		FluentCombo c = new FluentCombo();
		assertThat(c.get(), is(notNullValue()));
	}

	@Test
	void comboBindingSetsBinding() {
		FluentCombo c = new FluentCombo();
		c.binding("status");
		assertThat(c.get().getBinding(), is("status"));
	}

	@Test
	void comboPixelWidthSetsValue() {
		FluentCombo c = new FluentCombo();
		c.pixelWidth(150);
		assertEquals(150, c.get().getPixelWidth());
	}

	// ---- FluentButton ----

	@Test
	void buttonDefaultConstructorCreatesInstance() {
		FluentButton b = new FluentButton();
		assertThat(b.get(), is(notNullValue()));
	}

	@Test
	void buttonActionNameSetsValue() {
		FluentButton b = new FluentButton();
		b.actionName("Save");
		assertThat(b.get().getActionName(), is("Save"));
	}

	@Test
	void buttonPixelWidthSetsValue() {
		FluentButton b = new FluentButton();
		b.pixelWidth(100);
		assertEquals(100, b.get().getPixelWidth());
	}

	@Test
	void buttonPixelHeightSetsValue() {
		FluentButton b = new FluentButton();
		b.pixelHeight(40);
		assertEquals(40, b.get().getPixelHeight());
	}

	@Test
	void buttonMinPixelHeightSetsValue() {
		FluentButton b = new FluentButton();
		b.minPixelHeight(20);
		assertEquals(20, b.get().getMinPixelHeight());
	}

	@Test
	void buttonMaxPixelHeightSetsValue() {
		FluentButton b = new FluentButton();
		b.maxPixelHeight(60);
		assertEquals(60, b.get().getMaxPixelHeight());
	}

	@Test
	void buttonShowSetsValue() {
		FluentButton b = new FluentButton();
		b.show(ActionShow.icon);
		assertThat(b.get().getShow(), is(ActionShow.icon));
	}

	// ---- FluentLabel ----

	@Test
	void labelDefaultConstructorCreatesInstance() {
		FluentLabel l = new FluentLabel();
		assertThat(l.get(), is(notNullValue()));
	}

	@Test
	void labelValueSetsValue() {
		FluentLabel l = new FluentLabel();
		l.value("My Label");
		assertThat(l.get().getValue(), is("My Label"));
	}

	@Test
	void labelForBindingSetsValue() {
		FluentLabel l = new FluentLabel();
		l.forBinding("name");
		assertThat(l.get().getFor(), is("name"));
	}

	@Test
	void labelInvisibleConditionSetsValue() {
		FluentLabel l = new FluentLabel();
		l.invisibleConditionName("hidden");
		assertThat(l.get().getInvisibleConditionName(), is("hidden"));
	}

	@Test
	void labelFormattedSetsTrue() {
		FluentLabel l = new FluentLabel();
		l.formatted(true);
		assertThat(l.get().getFormatted(), is(Boolean.TRUE));
	}

	@Test
	void labelTextAlignmentSetsValue() {
		FluentLabel l = new FluentLabel();
		l.textAlignment(HorizontalAlignment.centre);
		assertThat(l.get().getTextAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	void labelEscapeSetsTrue() {
		FluentLabel l = new FluentLabel();
		l.escape(true);
		assertThat(l.get().getEscape(), is(Boolean.TRUE));
	}

	@Test
	void labelSanitiseSetsValue() {
		FluentLabel l = new FluentLabel();
		l.sanitise(Sanitisation.basic);
		assertThat(l.get().getSanitise(), is(Sanitisation.basic));
	}

	@Test
	void labelPixelWidthSetsValue() {
		FluentLabel l = new FluentLabel();
		l.pixelWidth(120);
		assertEquals(120, l.get().getPixelWidth());
	}

	@Test
	void labelPixelHeightSetsValue() {
		FluentLabel l = new FluentLabel();
		l.pixelHeight(24);
		assertEquals(24, l.get().getPixelHeight());
	}

	// ---- FluentBlurb ----

	@Test
	void blurbDefaultConstructorCreatesInstance() {
		FluentBlurb b = new FluentBlurb();
		assertThat(b.get(), is(notNullValue()));
	}

	@Test
	void blurbMarkupSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.markup("<b>Hello</b>");
		assertThat(b.get().getMarkup(), is("<b>Hello</b>"));
	}

	@Test
	void blurbInvisibleConditionSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.invisibleConditionName("noShow");
		assertThat(b.get().getInvisibleConditionName(), is("noShow"));
	}

	@Test
	void blurbTextAlignmentSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.textAlignment(HorizontalAlignment.right);
		assertThat(b.get().getTextAlignment(), is(HorizontalAlignment.right));
	}

	@Test
	void blurbEscapeSetsTrue() {
		FluentBlurb b = new FluentBlurb();
		b.escape(true);
		assertThat(b.get().getEscape(), is(Boolean.TRUE));
	}

	@Test
	void blurbSanitiseSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.sanitise(Sanitisation.relaxed);
		assertThat(b.get().getSanitise(), is(Sanitisation.relaxed));
	}

	@Test
	void blurbPixelWidthSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.pixelWidth(300);
		assertEquals(300, b.get().getPixelWidth());
	}

	@Test
	void blurbPixelHeightSetsValue() {
		FluentBlurb b = new FluentBlurb();
		b.pixelHeight(100);
		assertEquals(100, b.get().getPixelHeight());
	}

	@Test
	void blurbPutPropertyAddsEntry() {
		FluentBlurb b = new FluentBlurb();
		b.putProperty("key1", "val1");
		assertThat(b.get().getProperties().get("key1"), is("val1"));
	}

	// ---- FluentSpacer ----

	@Test
	void spacerDefaultConstructorCreatesInstance() {
		FluentSpacer s = new FluentSpacer();
		assertThat(s.get(), is(notNullValue()));
	}

	@Test
	void spacerInvisibleConditionSetsValue() {
		FluentSpacer s = new FluentSpacer();
		s.invisibleConditionName("hide");
		assertThat(s.get().getInvisibleConditionName(), is("hide"));
	}

	@Test
	void spacerPixelWidthSetsValue() {
		FluentSpacer s = new FluentSpacer();
		s.pixelWidth(10);
		assertEquals(10, s.get().getPixelWidth());
	}

	@Test
	void spacerPixelHeightSetsValue() {
		FluentSpacer s = new FluentSpacer();
		s.pixelHeight(10);
		assertEquals(10, s.get().getPixelHeight());
	}

	// ---- FluentTab ----

	@Test
	void tabDefaultConstructorCreatesInstance() {
		FluentTab t = new FluentTab();
		assertThat(t.get(), is(notNullValue()));
	}

	@Test
	void tabTitleSetsValue() {
		FluentTab t = new FluentTab();
		t.title("Details");
		assertThat(t.get().getTitle(), is("Details"));
	}

	@Test
	void tabIcon16x16SetsValue() {
		FluentTab t = new FluentTab();
		t.icon16x16RelativeFileName("icon.png");
		assertThat(t.get().getIcon16x16RelativeFileName(), is("icon.png"));
	}

	@Test
	void tabIconStyleClassSetsValue() {
		FluentTab t = new FluentTab();
		t.iconStyleClass("fa fa-home");
		assertThat(t.get().getIconStyleClass(), is("fa fa-home"));
	}

	@Test
	void tabDisabledConditionSetsValue() {
		FluentTab t = new FluentTab();
		t.disabledConditionName("isReadOnly");
		assertThat(t.get().getDisabledConditionName(), is("isReadOnly"));
	}

	@Test
	void tabInvisibleConditionSetsValue() {
		FluentTab t = new FluentTab();
		t.invisibleConditionName("isHidden");
		assertThat(t.get().getInvisibleConditionName(), is("isHidden"));
	}

	// ---- FluentTabPane ----

	@Test
	void tabPaneDefaultConstructorCreatesInstance() {
		FluentTabPane tp = new FluentTabPane();
		assertThat(tp.get(), is(notNullValue()));
	}

	@Test
	void tabPaneWidgetIdSetsValue() {
		FluentTabPane tp = new FluentTabPane();
		tp.widgetId("tabPane1");
		assertThat(tp.get().getWidgetId(), is("tabPane1"));
	}

	@Test
	void tabPanePixelWidthSetsValue() {
		FluentTabPane tp = new FluentTabPane();
		tp.pixelWidth(600);
		assertEquals(600, tp.get().getPixelWidth());
	}

	@Test
	void tabPaneDisabledConditionSetsValue() {
		FluentTabPane tp = new FluentTabPane();
		tp.disabledConditionName("disable");
		assertThat(tp.get().getDisabledConditionName(), is("disable"));
	}

	@Test
	void tabPaneInvisibleConditionSetsValue() {
		FluentTabPane tp = new FluentTabPane();
		tp.invisibleConditionName("hide");
		assertThat(tp.get().getInvisibleConditionName(), is("hide"));
	}

	@Test
	void tabPaneSelectedTabIndexBindingSetsValue() {
		FluentTabPane tp = new FluentTabPane();
		tp.selectedTabIndexBinding("selectedTab");
		assertThat(tp.get().getSelectedTabIndexBinding(), is("selectedTab"));
	}

	@Test
	void tabPaneAddTabAddsTab() {
		FluentTabPane tp = new FluentTabPane();
		tp.addTab(new FluentTab().title("Tab1"));
		assertEquals(1, tp.get().getTabs().size());
		assertThat(tp.get().getTabs().get(0).getTitle(), is("Tab1"));
	}

	// ---- FluentHBox ----

	@Test
	void hboxDefaultConstructorCreatesInstance() {
		FluentHBox hb = new FluentHBox();
		assertThat(hb.get(), is(notNullValue()));
	}

	@Test
	void hboxWidgetIdSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.widgetId("hbox1");
		assertThat(hb.get().getWidgetId(), is("hbox1"));
	}

	@Test
	void hboxBorderSetsTrue() {
		FluentHBox hb = new FluentHBox();
		hb.border(true);
		assertThat(hb.get().getBorder(), is(Boolean.TRUE));
	}

	@Test
	void hboxBorderTitleSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.borderTitle("Section");
		assertThat(hb.get().getBorderTitle(), is("Section"));
	}

	@Test
	void hboxCollapsibleSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.collapsible(Collapsible.open);
		assertThat(hb.get().getCollapsible(), is(Collapsible.open));
	}

	@Test
	void hboxPixelWidthSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.pixelWidth(400);
		assertEquals(400, hb.get().getPixelWidth());
	}

	@Test
	void hboxResponsiveWidthSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.responsiveWidth(6);
		assertEquals(6, hb.get().getResponsiveWidth());
	}

	@Test
	void hboxSmSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.sm(4);
		assertEquals(4, hb.get().getSm());
	}

	@Test
	void hboxMdSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.md(6);
		assertEquals(6, hb.get().getMd());
	}

	@Test
	void hboxLgSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.lg(8);
		assertEquals(8, hb.get().getLg());
	}

	@Test
	void hboxXlSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.xl(10);
		assertEquals(10, hb.get().getXl());
	}

	@Test
	void hboxPercentageWidthSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.percentageWidth(50);
		assertEquals(50, hb.get().getPercentageWidth());
	}

	@Test
	void hboxPixelHeightSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.pixelHeight(200);
		assertEquals(200, hb.get().getPixelHeight());
	}

	@Test
	void hboxPercentageHeightSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.percentageHeight(100);
		assertEquals(100, hb.get().getPercentageHeight());
	}

	@Test
	void hboxPixelPaddingSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.pixelPadding(5);
		assertEquals(5, hb.get().getPixelPadding());
	}

	@Test
	void hboxPixelMemberPaddingSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.pixelMemberPadding(3);
		assertEquals(3, hb.get().getPixelMemberPadding());
	}

	@Test
	void hboxShrinkWrapSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.shrinkWrap(ShrinkWrap.both);
		assertThat(hb.get().getShrinkWrap(), is(ShrinkWrap.both));
	}

	@Test
	void hboxHorizontalAlignmentSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.horizontalAlignment(HorizontalAlignment.centre);
		assertThat(hb.get().getHorizontalAlignment(), is(HorizontalAlignment.centre));
	}

	@Test
	void hboxVerticalAlignmentSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.verticalAlignment(VerticalAlignment.top);
		assertThat(hb.get().getVerticalAlignment(), is(VerticalAlignment.top));
	}

	@Test
	void hboxInvisibleConditionSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.invisibleConditionName("hidden");
		assertThat(hb.get().getInvisibleConditionName(), is("hidden"));
	}

	@Test
	void hboxMinPixelWidthSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.minPixelWidth(100);
		assertEquals(100, hb.get().getMinPixelWidth());
	}

	@Test
	void hboxMaxPixelWidthSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.maxPixelWidth(800);
		assertEquals(800, hb.get().getMaxPixelWidth());
	}

	@Test
	void hboxMinPixelHeightSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.minPixelHeight(50);
		assertEquals(50, hb.get().getMinPixelHeight());
	}

	@Test
	void hboxMaxPixelHeightSetsValue() {
		FluentHBox hb = new FluentHBox();
		hb.maxPixelHeight(500);
		assertEquals(500, hb.get().getMaxPixelHeight());
	}

	// ---- FluentVBox ----

	@Test
	void vboxDefaultConstructorCreatesInstance() {
		FluentVBox vb = new FluentVBox();
		assertThat(vb.get(), is(notNullValue()));
	}

	@Test
	void vboxWidgetIdSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.widgetId("vbox1");
		assertThat(vb.get().getWidgetId(), is("vbox1"));
	}

	@Test
	void vboxBorderSetsFalse() {
		FluentVBox vb = new FluentVBox();
		vb.border(false);
		assertThat(vb.get().getBorder(), is(Boolean.FALSE));
	}

	@Test
	void vboxBorderTitleSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.borderTitle("Panel");
		assertThat(vb.get().getBorderTitle(), is("Panel"));
	}

	@Test
	void vboxCollapsibleSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.collapsible(Collapsible.closed);
		assertThat(vb.get().getCollapsible(), is(Collapsible.closed));
	}

	@Test
	void vboxPixelWidthSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.pixelWidth(300);
		assertEquals(300, vb.get().getPixelWidth());
	}

	@Test
	void vboxPixelHeightSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.pixelHeight(150);
		assertEquals(150, vb.get().getPixelHeight());
	}

	@Test
	void vboxHorizontalAlignmentSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.horizontalAlignment(HorizontalAlignment.left);
		assertThat(vb.get().getHorizontalAlignment(), is(HorizontalAlignment.left));
	}

	@Test
	void vboxVerticalAlignmentSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.verticalAlignment(VerticalAlignment.middle);
		assertThat(vb.get().getVerticalAlignment(), is(VerticalAlignment.middle));
	}

	@Test
	void vboxInvisibleConditionSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.invisibleConditionName("invisible");
		assertThat(vb.get().getInvisibleConditionName(), is("invisible"));
	}

	@Test
	void vboxShrinkWrapSetsValue() {
		FluentVBox vb = new FluentVBox();
		vb.shrinkWrap(ShrinkWrap.both);
		assertThat(vb.get().getShrinkWrap(), is(ShrinkWrap.both));
	}

	// ---- FluentForm ----

	@Test
	void formDefaultConstructorCreatesInstance() {
		FluentForm f = new FluentForm();
		assertThat(f.get(), is(notNullValue()));
	}

	@Test
	void formWidgetIdSetsValue() {
		FluentForm f = new FluentForm();
		f.widgetId("form1");
		assertThat(f.get().getWidgetId(), is("form1"));
	}

	@Test
	void formBorderSetsTrue() {
		FluentForm f = new FluentForm();
		f.border(true);
		assertThat(f.get().getBorder(), is(Boolean.TRUE));
	}

	@Test
	void formBorderTitleSetsValue() {
		FluentForm f = new FluentForm();
		f.borderTitle("Form Title");
		assertThat(f.get().getBorderTitle(), is("Form Title"));
	}

	@Test
	void formPixelWidthSetsValue() {
		FluentForm f = new FluentForm();
		f.pixelWidth(500);
		assertEquals(500, f.get().getPixelWidth());
	}

	@Test
	void formResponsiveWidthSetsValue() {
		FluentForm f = new FluentForm();
		f.responsiveWidth(8);
		assertEquals(8, f.get().getResponsiveWidth());
	}

	@Test
	void formDisabledConditionSetsValue() {
		FluentForm f = new FluentForm();
		f.disabledConditionName("readOnly");
		assertThat(f.get().getDisabledConditionName(), is("readOnly"));
	}

	@Test
	void formInvisibleConditionSetsValue() {
		FluentForm f = new FluentForm();
		f.invisibleConditionName("hideForm");
		assertThat(f.get().getInvisibleConditionName(), is("hideForm"));
	}

	@Test
	void formAddRowAddsRow() {
		FluentForm f = new FluentForm();
		f.addRow(new FluentFormRow());
		assertEquals(1, f.get().getRows().size());
	}

	@Test
	void formAddColumnAddsColumn() {
		FluentForm f = new FluentForm();
		f.addColumn(new FluentFormColumn());
		assertEquals(1, f.get().getColumns().size());
	}

	// ---- FluentFormRow ----

	@Test
	void formRowDefaultConstructorCreatesInstance() {
		FluentFormRow r = new FluentFormRow();
		assertThat(r.get(), is(notNullValue()));
	}

	@Test
	void formRowAddItemAddsItem() {
		FluentFormRow r = new FluentFormRow();
		r.addItem(new FluentFormItem());
		assertEquals(1, r.get().getItems().size());
	}

	// ---- FluentFormItem ----

	@Test
	void formItemDefaultConstructorCreatesInstance() {
		FluentFormItem fi = new FluentFormItem();
		assertThat(fi.get(), is(notNullValue()));
	}

	@Test
	void formItemLabelSetsValue() {
		FluentFormItem fi = new FluentFormItem();
		fi.label("Name");
		assertThat(fi.get().getLabel(), is("Name"));
	}

	@Test
	void formItemColspanSetsValue() {
		FluentFormItem fi = new FluentFormItem();
		fi.colspan(2);
		assertThat(fi.get().getColspan(), is(Integer.valueOf(2)));
	}

	@Test
	void formItemRequiredSetsTrue() {
		FluentFormItem fi = new FluentFormItem();
		fi.required(true);
		assertThat(fi.get().getRequired(), is(Boolean.TRUE));
	}

	@Test
	void formItemRequiredSetsFalse() {
		FluentFormItem fi = new FluentFormItem();
		fi.required(false);
		assertThat(fi.get().getRequired(), is(Boolean.FALSE));
	}

	@Test
	void formItemWidgetSetsValue() {
		FluentFormItem fi = new FluentFormItem();
		FluentTextField tf = new FluentTextField().binding("name");
		fi.textField(tf);
		assertThat(fi.get().getWidget(), is(notNullValue()));
	}

	// ---- FluentFormColumn ----

	@Test
	void formColumnDefaultConstructorCreatesInstance() {
		FluentFormColumn fc = new FluentFormColumn();
		assertThat(fc.get(), is(notNullValue()));
	}

	@Test
	void formColumnPercentageWidthSetsValue() {
		FluentFormColumn fc = new FluentFormColumn();
		fc.percentageWidth(50);
		assertEquals(50, fc.get().getPercentageWidth());
	}

	// ---- FluentDataGrid ----

	@Test
	void dataGridDefaultConstructorCreatesInstance() {
		FluentDataGrid dg = new FluentDataGrid();
		assertThat(dg.get(), is(notNullValue()));
	}

	@Test
	void dataGridBindingSetsValue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.binding("items");
		assertThat(dg.get().getBinding(), is("items"));
	}

	@Test
	void dataGridDisabledConditionSetsValue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.disabledConditionName("isReadOnly");
		assertThat(dg.get().getDisabledConditionName(), is("isReadOnly"));
	}

	@Test
	void dataGridShowAddSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.showAdd(true);
		assertThat(dg.get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void dataGridShowEditSetsFalse() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.showEdit(false);
		assertThat(dg.get().getShowEdit(), is(Boolean.FALSE));
	}

	@Test
	void dataGridShowZoomSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.showZoom(true);
		assertThat(dg.get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void dataGridShowRemoveSetsFalse() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.showRemove(false);
		assertThat(dg.get().getShowRemove(), is(Boolean.FALSE));
	}

	@Test
	void dataGridShowDeselectSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.showDeselect(true);
		assertThat(dg.get().getShowDeselect(), is(Boolean.TRUE));
	}

	@Test
	void dataGridSelectedIdBindingSetsValue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.selectedIdBinding("selectedId");
		assertThat(dg.get().getSelectedIdBinding(), is("selectedId"));
	}

	@Test
	void dataGridInlineSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.inline(true);
		assertThat(dg.get().getInline(), is(Boolean.TRUE));
	}

	@Test
	void dataGridEditableSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.editable(true);
		assertThat(dg.get().getEditable(), is(Boolean.TRUE));
	}

	@Test
	void dataGridWordWrapSetsTrue() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.wordWrap(true);
		assertThat(dg.get().getWordWrap(), is(Boolean.TRUE));
	}

	@Test
	void dataGridAddBoundColumnAddsColumn() {
		FluentDataGrid dg = new FluentDataGrid();
		dg.addBoundColumn(new FluentDataGridBoundColumn().binding("col1"));
		assertEquals(1, dg.get().getColumns().size());
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

	// ---- FluentListGrid ----

	@Test
	void listGridDefaultConstructorCreatesInstance() {
		FluentListGrid lg = new FluentListGrid();
		assertThat(lg.get(), is(notNullValue()));
	}

	@Test
	void listGridTitleSetsValue() {
		FluentListGrid lg = new FluentListGrid();
		lg.title("My Grid");
		assertThat(lg.get().getTitle(), is("My Grid"));
	}

	@Test
	void listGridQueryNameSetsValue() {
		FluentListGrid lg = new FluentListGrid();
		lg.queryName("qContacts");
		assertThat(lg.get().getQueryName(), is("qContacts"));
	}

	@Test
	void listGridModelNameSetsValue() {
		FluentListGrid lg = new FluentListGrid();
		lg.modelName("myModel");
		assertThat(lg.get().getModelName(), is("myModel"));
	}

	@Test
	void listGridShowAddSetsTrue() {
		FluentListGrid lg = new FluentListGrid();
		lg.showAdd(true);
		assertThat(lg.get().getShowAdd(), is(Boolean.TRUE));
	}

	@Test
	void listGridShowEditSetsTrue() {
		FluentListGrid lg = new FluentListGrid();
		lg.showEdit(true);
		assertThat(lg.get().getShowEdit(), is(Boolean.TRUE));
	}

	@Test
	void listGridShowZoomSetsTrue() {
		FluentListGrid lg = new FluentListGrid();
		lg.showZoom(true);
		assertThat(lg.get().getShowZoom(), is(Boolean.TRUE));
	}

	@Test
	void listGridShowRemoveSetsFalse() {
		FluentListGrid lg = new FluentListGrid();
		lg.showRemove(false);
		assertThat(lg.get().getShowRemove(), is(Boolean.FALSE));
	}

	@Test
	void listGridDisabledConditionSetsValue() {
		FluentListGrid lg = new FluentListGrid();
		lg.disabledConditionName("noEdit");
		assertThat(lg.get().getDisabledConditionName(), is("noEdit"));
	}

	@Test
	void listGridPixelWidthSetsValue() {
		FluentListGrid lg = new FluentListGrid();
		lg.pixelWidth(700);
		assertEquals(700, lg.get().getPixelWidth());
	}

	// ---- FluentView ----

	@Test
	void viewDefaultConstructorCreatesInstance() {
		FluentView v = new FluentView();
		assertThat(v.get(), is(notNullValue()));
	}

	@Test
	void viewNameSetsValue() {
		FluentView v = new FluentView();
		v.name("edit");
		assertThat(v.get().getName(), is("edit"));
	}

	@Test
	void viewTitleSetsValue() {
		FluentView v = new FluentView();
		v.title("Edit Contact");
		assertThat(v.get().getTitle(), is("Edit Contact"));
	}

	@Test
	void viewIconStyleClassSetsValue() {
		FluentView v = new FluentView();
		v.iconStyleClass("fa fa-user");
		assertThat(v.get().getIconStyleClass(), is("fa fa-user"));
	}

	@Test
	void viewDocumentationSetsValue() {
		FluentView v = new FluentView();
		v.documentation("Some doc");
		assertThat(v.get().getDocumentation(), is("Some doc"));
	}

	@Test
	void viewHelpRelativeFileNameSetsValue() {
		FluentView v = new FluentView();
		v.helpRelativeFileName("help.html");
		assertThat(v.get().getHelpRelativeFileName(), is("help.html"));
	}

	@Test
	void viewHelpURLSetsValue() {
		FluentView v = new FluentView();
		v.helpURL("http://example.com/help");
		assertThat(v.get().getHelpURL(), is("http://example.com/help"));
	}

	@Test
	void viewRefreshTimeInSecondsSetsValue() {
		FluentView v = new FluentView();
		v.refreshTimeInSeconds(30);
		assertEquals(30, v.get().getRefreshTimeInSeconds());
	}

	@Test
	void viewRefreshConditionNameSetsValue() {
		FluentView v = new FluentView();
		v.refreshConditionName("needsRefresh");
		assertThat(v.get().getRefreshConditionName(), is("needsRefresh"));
	}

	@Test
	void viewRefreshActionNameSetsValue() {
		FluentView v = new FluentView();
		v.refreshActionName("onRefresh");
		assertThat(v.get().getRefreshActionName(), is("onRefresh"));
	}
}
