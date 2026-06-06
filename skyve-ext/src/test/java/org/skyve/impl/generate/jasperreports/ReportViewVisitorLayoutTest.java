package org.skyve.impl.generate.jasperreports;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.lang.reflect.Method;
import java.util.ArrayList;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.types.Decimal5;
import org.skyve.impl.generate.jasperreports.Container.ContainerType;
import org.skyve.impl.generate.jasperreports.ReportElement.ElementType;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.container.HBox;
import org.skyve.impl.metadata.view.container.Tab;
import org.skyve.impl.metadata.view.container.VBox;
import org.skyve.impl.metadata.view.container.form.Form;
import org.skyve.impl.metadata.view.container.form.FormColumn;
import org.skyve.impl.metadata.view.container.form.FormItem;
import org.skyve.impl.metadata.view.container.form.FormRow;
import org.skyve.impl.metadata.view.widget.DynamicImage;
import org.skyve.impl.metadata.view.widget.Spacer;
import org.skyve.impl.metadata.view.widget.StaticImage;
import org.skyve.impl.metadata.view.widget.bound.input.ListMembership;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.ListRepeater;

@SuppressWarnings("boxing")
class ReportViewVisitorLayoutTest {
	private TestVisitor visitor;
	private DesignSpecification design;

	@BeforeEach
	void setUp() {
		design = new DesignSpecification();
		design.setColumnWidth(Integer.valueOf(1200));
		design.setDefaultElementHeight(Integer.valueOf(20));
		design.setPixelToTwip(new Decimal5("10"));
		design.setSectionBorderTop(Boolean.FALSE);
		design.setSectionBorderLeft(Boolean.FALSE);
		design.setSectionBorderBottom(Boolean.FALSE);
		design.setSectionBorderRight(Boolean.FALSE);
		visitor = new TestVisitor();
		visitor.setDesign(design);
		visitor.detailBands = new ArrayList<>();
		visitor.band = new ReportBand();
		visitor.band.setParent(design);
	}

	@Test
	void accessorsAndDefaultTwipConversionUseVisitorState() throws Exception {
		assertThat(visitor.isVisited(), is(false));
		visitor.setVisited(true);
		visitor.viewTitle = "Accounts";
		visitor.setShowLabel(Boolean.TRUE);
		visitor.detailBands.add(new ReportBand());
		design.setPixelToTwip(null);

		assertThat(visitor.isVisited(), is(true));
		assertThat(visitor.getViewTitle(), is("Accounts"));
		assertThat(visitor.getShowLabel(), is(Boolean.TRUE));
		assertThat(visitor.getDetailBands().size(), is(1));
		assertThat(invokeCalculateTwipSize(visitor, Integer.valueOf(1200), Integer.valueOf(4), null, null), is(Integer.valueOf(60)));
		assertThat(invokeCalculateTwipSize(visitor, Integer.valueOf(1200), null, null, null), nullValue());
	}

	@Test
	void startAndFinishDetailBandAddsLaidOutContainerWhenElementsExist() {
		visitor.startDetailBand(null, null, "hiddenWhen");
		Container container = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(500), false, ContainerType.vbox);
		container.getElements().add(new ReportElement(ElementType.textField, "name", "$F{name}", null, null, null, null, null, Integer.valueOf(25), null, null, null, null, null));
		visitor.currentContainer = container;

		visitor.finishDetailBand();

		assertThat(visitor.getDetailBands().size(), is(1));
		assertThat(visitor.getDetailBands().get(0).getName(), is("Detail0"));
		assertThat(visitor.getDetailBands().get(0).getInvisibleConditionName(), is("hiddenWhen"));
		assertThat(visitor.band, nullValue());
	}

	@Test
	void addContainerAndHandleEndContainerMaintainNestedState() {
		visitor.addContainer("section", "Section", Boolean.TRUE, Integer.valueOf(20), null, null, Boolean.FALSE, ContainerType.vbox, null);
		Container root = visitor.currentContainer;
		visitor.addContainer("child", null, Boolean.FALSE, null, Integer.valueOf(50), null, Boolean.TRUE, ContainerType.hbox, null);

		assertThat(visitor.containerDepth, is(2));
		assertThat(visitor.currentContainer.parent, is(root));
		assertThat(root.getContainers().size(), is(1));

		visitor.handleEndContainer();
		assertThat(visitor.containerDepth, is(1));
		assertThat(visitor.currentContainer, is(root));
	}

	@Test
	void visitImageWidgetsAppendNonFieldElements() {
		visitor.currentContainer = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(800), false, ContainerType.vbox);
		DynamicImage dynamicImage = new DynamicImage();
		dynamicImage.setName("chart");
		dynamicImage.setPixelWidth(Integer.valueOf(120));
		dynamicImage.setInvisibleConditionName("hideChart");
		StaticImage staticImage = new StaticImage();
		staticImage.setRelativeFile("logo.png");
		staticImage.setPixelWidth(Integer.valueOf(80));
		staticImage.setInvisibleConditionName("hideLogo");

		visitor.visitDynamicImage(dynamicImage, true, true);
		visitor.visitStaticImage(staticImage, true, true);

		assertThat(visitor.currentContainer.getElements().size(), is(2));
		assertThat(visitor.currentContainer.getElements().get(0).getElementType(), is(ElementType.dynamicImage));
		assertThat(visitor.currentContainer.getElements().get(0).getElementValue(), is("chart"));
		assertThat(visitor.currentContainer.getElements().get(0).getPixelWidth(), is(Integer.valueOf(120)));
		assertThat(visitor.currentContainer.getElements().get(1).getElementType(), is(ElementType.staticImage));
		assertThat(visitor.currentContainer.getElements().get(1).getElementValue(), is("logo.png"));
	}

	@Test
	void visitContainerAndStateOnlyWidgetsUpdateTraversalState() {
		Form form = new Form();
		form.setWidgetId("editForm");
		form.setBorder(Boolean.TRUE);
		form.setBorderTitle("Edit");
		visitor.visitForm(form, true, true);

		assertThat(visitor.inForm, is(true));
		assertThat(visitor.row, is(0));
		assertThat(visitor.currentContainer.getContainerType(), is(ContainerType.form));

		design.setVerticalise(Boolean.TRUE);
		HBox hbox = new HBox();
		hbox.setWidgetId("box");
		visitor.visitHBox(hbox, true, true);
		assertThat(visitor.currentContainer.getHorizontal(), is(Boolean.FALSE));

		visitor.col = 2;
		visitor.visitSpacer(new Spacer());
		assertThat(visitor.col, is(3));

		visitor.visitListGrid(new ListGrid(), true, true);
		assertThat(visitor.subreport, is(true));
		visitor.subreport = false;
		visitor.visitListRepeater(new ListRepeater(), true, true);
		assertThat(visitor.subreport, is(true));
		visitor.subreport = false;
		visitor.visitListMembership(new ListMembership(), true, true);
		assertThat(visitor.subreport, is(true));
	}

	@Test
	void visitFormColumnItemRowAndCompletionMaintainFormState() {
		Form form = new Form();
		form.setWidgetId("details");
		visitor.visitForm(form, true, true);
		Container formContainer = visitor.currentContainer;
		FormColumn formColumn = new FormColumn();
		formColumn.setPercentageWidth(Integer.valueOf(50));

		visitor.visitFormColumn(formColumn, true, true);
		FormItem item = new FormItem();
		item.setShowLabel(Boolean.FALSE);
		visitor.visitFormItem(item, true, true);
		visitor.col = 3;
		visitor.left = 75;
		visitor.visitFormRow(new FormRow(), true, true);

		assertThat(formContainer.getContainers().size(), is(1));
		assertThat(formContainer.getContainers().get(0).getPercentageWidth(), is(Integer.valueOf(50)));
		assertThat(visitor.currentContainer, is(formContainer));
		assertThat(visitor.getShowLabel(), is(Boolean.FALSE));
		assertThat(visitor.col, is(0));
		assertThat(visitor.left, is(0));

		ReportElement element = new ReportElement(ElementType.textField, "name", "$F{name}", null, null, null, null, null, Integer.valueOf(20), null, null, null, null, null);
		element.setRow(Integer.valueOf(0));
		formContainer.getContainers().get(0).getElements().add(element);
		formContainer.addContainer(new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(100), false, ContainerType.column));

		visitor.visitedFormRow(new FormRow(), true, true);
		assertThat(visitor.row, is(1));
		assertThat(formContainer.getRows(), is(1));

		visitor.visitedForm(form, true, true);
		assertThat(formContainer.getContainers().size(), is(1));
		assertThat(visitor.inForm, is(false));
	}

	@Test
	void visitAndCompleteSimpleContainersCloseCurrentBand() {
		VBox vbox = new VBox();
		vbox.setWidgetId("vertical");
		vbox.setBorder(Boolean.TRUE);
		vbox.setBorderTitle("Vertical");

		visitor.visitVBox(vbox, true, true);
		assertThat(visitor.currentContainer.getContainerType(), is(ContainerType.vbox));
		visitor.currentContainer.getElements().add(new ReportElement(ElementType.staticImage, "logo", "logo.png", null, null, null, null, null, Integer.valueOf(20), null, null, null, null, null));
		visitor.visitedVBox(vbox, true, true);
		assertThat(visitor.currentContainer, nullValue());

		Tab tab = new Tab();
		tab.setTitle("General");
		tab.setInvisibleConditionName("hideTab");
		visitor.visitTab(tab, true, true);
		assertThat(visitor.currentContainer.getContainerType(), is(ContainerType.tab));
		assertThat(visitor.currentContainer.getBorderTitle(), is("General"));
		assertThat(visitor.band.getInvisibleConditionName(), is("hideTab"));
		visitor.currentContainer.getElements().add(new ReportElement(ElementType.staticImage, "icon", "icon.png", null, null, null, null, null, Integer.valueOf(20), null, null, null, null, null));
		visitor.visitedTab(tab, true, true);
		assertThat(visitor.currentContainer, nullValue());
	}

	@Test
	void layoutColumnAppliesWidthHintsAndFillsEmptyRows() {
		Container column = new Container(Integer.valueOf(0), Integer.valueOf(0), null, false, ContainerType.column);
		column.setRows(3);
		column.setPercentageWidth(Integer.valueOf(50));
		ReportElement first = new ReportElement(ElementType.textField, "first", "$F{first}", null, null, null, null, null, null, null, null, null, null, null);
		first.setRow(Integer.valueOf(0));
		ReportElement third = new ReportElement(ElementType.textField, "third", "$F{third}", null, null, null, null, null, Integer.valueOf(30), null, null, null, null, null);
		third.setRow(Integer.valueOf(2));
		column.getElements().add(first);
		column.getElements().add(third);

		visitor.layout(column);

		assertThat(column.getWidth(), is(Integer.valueOf(600)));
		assertThat(column.getHeight(), is(Integer.valueOf(70)));
		assertThat(first.getElementTop(), is(Integer.valueOf(0)));
		assertThat(first.getElementHeight(), is(Integer.valueOf(20)));
		assertThat(third.getElementTop(), is(Integer.valueOf(40)));
		assertThat(third.getElementHeight(), is(Integer.valueOf(30)));
	}

	@Test
	void layoutLeafContainerSizesMixedElementsHorizontally() {
		Container hbox = new Container(Integer.valueOf(0), Integer.valueOf(10), Integer.valueOf(900), true, ContainerType.hbox);
		ReportElement fixed = new ReportElement(ElementType.staticImage, "fixed", "fixed.png", null, null, null, null, Integer.valueOf(200), null, null, null, null, null, null);
		fixed.setPixelWidth(Integer.valueOf(20));
		ReportElement auto = new ReportElement(ElementType.dynamicImage, "auto", "chart", null, null, null, null, null, null, null, null, null, null, null);
		hbox.getElements().add(fixed);
		hbox.getElements().add(auto);

		visitor.layout(hbox);

		assertThat(fixed.getElementLeft(), is(Integer.valueOf(10)));
		assertThat(fixed.getElementWidth(), is(Integer.valueOf(200)));
		assertThat(auto.getElementLeft(), is(Integer.valueOf(210)));
		assertThat(auto.getElementWidth(), is(Integer.valueOf(0)));
		assertThat(hbox.getLeft(), is(Integer.valueOf(210)));
		assertThat(hbox.getHeight(), is(Integer.valueOf(0)));
	}

	@Test
	void layoutNestedHorizontalContainerSplitsUnsizedChildren() {
		Container parent = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(1000), true, ContainerType.hbox);
		Container fixed = new Container(Integer.valueOf(0), Integer.valueOf(0), null, false, ContainerType.vbox);
		fixed.setResponsiveWidth(Integer.valueOf(3));
		Container autoOne = new Container(Integer.valueOf(0), Integer.valueOf(0), null, false, ContainerType.vbox);
		Container autoTwo = new Container(Integer.valueOf(0), Integer.valueOf(0), null, false, ContainerType.vbox);
		parent.addContainer(fixed);
		parent.addContainer(autoOne);
		parent.addContainer(autoTwo);

		visitor.layout(parent);

		assertThat(fixed.getWidth(), is(Integer.valueOf(250)));
		assertThat(autoOne.getWidth(), is(Integer.valueOf(375)));
		assertThat(autoTwo.getWidth(), is(Integer.valueOf(375)));
		assertThat(autoOne.getLeft(), is(Integer.valueOf(250)));
		assertThat(autoTwo.getLeft(), is(Integer.valueOf(625)));
	}

	@Test
	void layoutBorderedContainerAddsTitleAndBorderElements() {
		Container container = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(600), false, ContainerType.vbox);
		container.setBorder(Boolean.TRUE);
		container.setBorderTitle("Section");
		ReportElement element = new ReportElement(ElementType.dynamicImage, "image", "image", null, null, null, null, null, Integer.valueOf(40), null, null, null, null, null);
		container.getElements().add(element);

		visitor.layout(container);

		assertThat(visitor.band.getElements().get(0).getElementType(), is(ElementType.border));
		assertThat(visitor.band.getElements().get(1).getElementType(), is(ElementType.staticText));
		assertThat(visitor.band.getElements().get(1).getElementValue(), is("Section"));
		assertThat(element.getElementTop(), is(Integer.valueOf(20)));
	}

	@Test
	void layoutFormNormalisesRowHeightsAcrossColumns() {
		Container form = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(800), true, ContainerType.form);
		form.setRows(2);
		Container colOne = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), false, ContainerType.column);
		Container colTwo = new Container(Integer.valueOf(0), Integer.valueOf(0), Integer.valueOf(400), false, ContainerType.column);
		ReportElement rowZero = new ReportElement(ElementType.textField, "rowZero", "$F{rowZero}", null, null, null, null, null, Integer.valueOf(15), null, null, null, null, null);
		rowZero.setRow(Integer.valueOf(0));
		ReportElement rowOne = new ReportElement(ElementType.textField, "rowOne", "$F{rowOne}", null, null, null, null, null, Integer.valueOf(30), null, null, null, null, null);
		rowOne.setRow(Integer.valueOf(1));
		colOne.getElements().add(rowZero);
		colTwo.getElements().add(rowOne);
		form.addContainer(colOne);
		form.addContainer(colTwo);

		visitor.layout(form);

		assertThat(colOne.getHeight(), is(Integer.valueOf(45)));
		assertThat(colTwo.getHeight(), is(Integer.valueOf(45)));
		assertThat(rowOne.getElementTop(), is(Integer.valueOf(15)));
	}

	@Test
	void noOpVisitorHooksAndViewLifecycleAreSafe() {
		visitor.visitView();
		visitor.visitedView();
		assertThat(visitor.isVisited(), is(true));

		visitor.visitBlurb(null, true, true);
		visitor.visitButton(null, true, true);
		visitor.visitZoomIn(null, true, true);
		visitor.visitCheckMembership(null, true, true);
		visitor.visitComparison(null, true, true);
		visitor.visitContentLink(null, true, true);
		visitor.visitDataGridBoundColumn(null, true, true);
		visitor.visitDataGridContainerColumn(null, true, true);
		visitor.visitDialogButton(null, true, true);
		visitor.visitHTML(null, true, true);
		visitor.visitInject(null, true, true);
		visitor.visitLabel(null, true, true);
		visitor.visitLink(null, true, true);
		visitor.visitMap(null, true, true);
		visitor.visitChart(null, true, true);
		visitor.visitProgressBar(null, true, true);
		visitor.visitRerenderEventAction(null, null, true, true);
		visitor.visitServerSideActionEventAction(null, true, true);
		visitor.visitSetDisabledEventAction(null, true, true);
		visitor.visitSetInvisibleEventAction(null, true, true);
		visitor.visitTabPane(null, true, true);
		visitor.visitToggleDisabledEventAction(null, true, true);
		visitor.visitToggleVisibilityEventAction(null, true, true);
		visitor.visitTreeGrid(null, true, true);
		visitor.visitSidebar(null, true, true);
		visitor.visitedSidebar(null, true, true);

		visitor.visitOnAddedEventHandler(null, true, true);
		visitor.visitOnBlurEventHandler(null, true, true);
		visitor.visitOnChangedEventHandler(null, true, true);
		visitor.visitOnClearedEventHandler(null, true, true);
		visitor.visitOnEditedEventHandler(null, true, true);
		visitor.visitOnFocusEventHandler(null, true, true);
		visitor.visitOnPickedEventHandler(null, true, true);
		visitor.visitOnRemovedEventHandler(null, true, true);
		visitor.visitOnSelectedEventHandler(null, true, true);

		visitor.visitedCheckBox(null, true, true);
		visitor.visitedCheckMembership(null, true, true);
		visitor.visitedColourPicker(null, true, true);
		visitor.visitedCombo(null, true, true);
		visitor.visitedDataGridBoundColumn(null, true, true);
		visitor.visitedFormItem(null, true, true);
		visitor.visitedGeometry(null, true, true);
		visitor.visitedGeometryMap(null, true, true);
		visitor.visitedLookupDescription(null, true, true);
		visitor.visitedOnAddedEventHandler(null, true, true);
		visitor.visitedOnBlurEventHandler(null, true, true);
		visitor.visitedOnChangedEventHandler(null, true, true);
		visitor.visitedOnClearedEventHandler(null, true, true);
		visitor.visitedOnEditedEventHandler(null, true, true);
		visitor.visitedOnFocusEventHandler(null, true, true);
		visitor.visitedOnPickedEventHandler(null, true, true);
		visitor.visitedOnRemovedEventHandler(null, true, true);
		visitor.visitedOnSelectedEventHandler(null, true, true);
		visitor.visitedPassword(null, true, true);
		visitor.visitedRadio(null, true, true);
		visitor.visitedRichText(null, true, true);
		visitor.visitedSlider(null, true, true);
		visitor.visitedSpinner(null, true, true);
		visitor.visitedTabPane(null, true, true);
		visitor.visitedTextArea(null, true, true);
		visitor.visitedTextField(null, true, true);
		visitor.visitedTreeGrid(null, true, true);

		visitor.visitAddAction(null);
		visitor.visitBizExportAction(null);
		visitor.visitBizImportAction(null);
		visitor.visitCancelAction(null);
		visitor.visitCustomAction(null);
		visitor.visitDeleteAction(null);
		visitor.visitDownloadAction(null);
		visitor.visitEditAction(null);
		visitor.visitFilterParameter(null, true, true);
		visitor.visitNavigateAction(null);
		visitor.visitNewAction(null);
		visitor.visitOKAction(null);
		visitor.visitParameter(null, true, true);
		visitor.visitRemoveAction(null);
		visitor.visitReportAction(null);
		visitor.visitSaveAction(null);
		visitor.visitUploadAction(null);
		visitor.visitZoomOutAction(null);
	}

	private static final class TestVisitor extends ReportViewVisitor {
		private TestVisitor() {
			super(null, null, null, null, new ViewImpl(), "desktop");
		}
	}

	private static Integer invokeCalculateTwipSize(TestVisitor visitor, Integer containerSizeInTwips, Integer pixels, Integer percentage, Integer responsive) throws Exception {
		Method method = ReportViewVisitor.class.getDeclaredMethod("calculateTwipSize", Integer.class, Integer.class, Integer.class, Integer.class);
		method.setAccessible(true);
		return (Integer) method.invoke(visitor, containerSizeInTwips, pixels, percentage, responsive);
	}
}
