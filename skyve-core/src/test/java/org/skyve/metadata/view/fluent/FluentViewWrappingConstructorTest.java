package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.view.Actions;
import org.skyve.impl.metadata.repository.view.actions.UploadAction;
import org.skyve.impl.metadata.repository.view.ViewMetaData;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.widget.bound.input.ContentCapture;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.SetDisabledEventAction;
import org.skyve.impl.metadata.view.event.SetInvisibleEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.FilterParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ParameterImpl;
import org.skyve.impl.metadata.view.widget.bound.ZoomIn;
import org.skyve.impl.metadata.view.widget.bound.input.CheckBox;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.GeometryMap;
import org.skyve.impl.metadata.view.widget.bound.input.HTML;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.tabular.DataGrid;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

/**
 * Tests for fluent widget/event wrapping constructors not covered elsewhere.
 */
@SuppressWarnings("static-method")
class FluentViewWrappingConstructorTest {

	// ---- FluentColourPicker -------------------------------------------

	@Test
	void colourPickerWrappingConstructorPreservesInstance() {
		ColourPicker existing = new ColourPicker();
		FluentColourPicker fluent = new FluentColourPicker(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentCombo ---------------------------------------------------

	@Test
	void comboWrappingConstructorPreservesInstance() {
		Combo existing = new Combo();
		FluentCombo fluent = new FluentCombo(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentPassword -----------------------------------------------

	@Test
	void passwordWrappingConstructorPreservesInstance() {
		Password existing = new Password();
		FluentPassword fluent = new FluentPassword(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentInjectBinding ------------------------------------------

	@Test
	void injectBindingWrappingConstructorPreservesInstance() {
		InjectBinding existing = new InjectBinding();
		FluentInjectBinding fluent = new FluentInjectBinding(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentTreeGrid -----------------------------------------------

	@Test
	void treeGridWrappingConstructorPreservesInstance() {
		TreeGrid existing = new TreeGrid();
		FluentTreeGrid fluent = new FluentTreeGrid(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void treeGridRootIdBindingSetsValue() {
		FluentTreeGrid fluent = new FluentTreeGrid().rootIdBinding("parentBizId");
		assertEquals("parentBizId", fluent.get().getRootIdBinding());
	}

	// ---- FluentRadio --------------------------------------------------

	@Test
	void radioWrappingConstructorPreservesInstance() {
		Radio existing = new Radio();
		FluentRadio fluent = new FluentRadio(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void radioVerticalSetsValue() {
		FluentRadio fluent = new FluentRadio().vertical(true);
		assertNotNull(fluent.get().getVertical());
		assertEquals(Boolean.TRUE, fluent.get().getVertical());
	}

	// ---- FluentDefaultWidget ------------------------------------------

	@Test
	void defaultWidgetWrappingConstructorPreservesInstance() {
		DefaultWidget existing = new DefaultWidget();
		FluentDefaultWidget fluent = new FluentDefaultWidget(existing);
		assertSame(existing, fluent.get());
	}


	// ---- FluentContentUpload ------------------------------------------

	@Test
	void contentUploadWrappingConstructorPreservesInstance() {
		org.skyve.impl.metadata.view.widget.bound.input.ContentUpload existing =
				new org.skyve.impl.metadata.view.widget.bound.input.ContentUpload();
		FluentContentUpload fluent = new FluentContentUpload(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentUploadAction -------------------------------------------

	@Test
	void uploadActionCaptureSetsValue() {
		FluentUploadAction fluent = new FluentUploadAction().capture(ContentCapture.video);
		assertEquals(ContentCapture.video, fluent.get().getCapture());
	}

	@Test
	void uploadActionFromCopiesCapture() {
		UploadAction existing = new UploadAction();
		existing.setCapture(ContentCapture.camera);
		FluentUploadAction fluent = new FluentUploadAction().from(existing);
		assertEquals(ContentCapture.camera, fluent.get().getCapture());
	}

	// ---- FluentRerenderEventAction ------------------------------------

	@Test
	void rerenderWrappingConstructorPreservesInstance() {
		RerenderEventAction existing = new RerenderEventAction();
		FluentRerenderEventAction fluent = new FluentRerenderEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentToggleDisabledEventAction ------------------------------

	@Test
	void toggleDisabledWrappingConstructorPreservesInstance() {
		ToggleDisabledEventAction existing = new ToggleDisabledEventAction();
		FluentToggleDisabledEventAction fluent = new FluentToggleDisabledEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentToggleVisibilityEventAction ----------------------------

	@Test
	void toggleVisibilityWrappingConstructorPreservesInstance() {
		ToggleVisibilityEventAction existing = new ToggleVisibilityEventAction();
		FluentToggleVisibilityEventAction fluent = new FluentToggleVisibilityEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentServerSideActionEventAction ----------------------------

	@Test
	void serverSideActionWrappingConstructorPreservesInstance() {
		ServerSideActionEventAction existing = new ServerSideActionEventAction();
		FluentServerSideActionEventAction fluent = new FluentServerSideActionEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentSetDisabledEventAction ---------------------------------

	@Test
	void setDisabledWrappingConstructorPreservesInstance() {
		SetDisabledEventAction existing = new SetDisabledEventAction();
		FluentSetDisabledEventAction fluent = new FluentSetDisabledEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentSetInvisibleEventAction --------------------------------

	@Test
	void setInvisibleWrappingConstructorPreservesInstance() {
		SetInvisibleEventAction existing = new SetInvisibleEventAction();
		FluentSetInvisibleEventAction fluent = new FluentSetInvisibleEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentHTML ----------------------------------------------------

	@Test
	void htmlWrappingConstructorPreservesInstance() {
		HTML existing = new HTML();
		FluentHTML fluent = new FluentHTML(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentParameter -----------------------------------------------

	@Test
	void parameterWrappingConstructorPreservesInstance() {
		ParameterImpl existing = new ParameterImpl();
		FluentParameter fluent = new FluentParameter(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentFilterParameter -----------------------------------------

	@Test
	void filterParameterWrappingConstructorPreservesInstance() {
		FilterParameterImpl existing = new FilterParameterImpl();
		FluentFilterParameter fluent = new FluentFilterParameter(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentCheckBox ------------------------------------------------

	@Test
	void checkBoxWrappingConstructorPreservesInstance() {
		CheckBox existing = new CheckBox();
		FluentCheckBox fluent = new FluentCheckBox(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentGeometryMap ---------------------------------------------

	@Test
	void geometryMapWrappingConstructorPreservesInstance() {
		GeometryMap existing = new GeometryMap();
		FluentGeometryMap fluent = new FluentGeometryMap(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentZoomIn --------------------------------------------------

	@Test
	void zoomInWrappingConstructorPreservesInstance() {
		ZoomIn existing = new ZoomIn();
		FluentZoomIn fluent = new FluentZoomIn(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentDataGrid ------------------------------------------------

	@Test
	void dataGridWrappingConstructorPreservesInstance() {
		DataGrid existing = new DataGrid();
		FluentDataGrid fluent = new FluentDataGrid(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentView ----------------------------------------------------

	@Test
	void viewWrappingConstructorPreservesInstance() {
		ViewMetaData existing = new ViewMetaData();
		FluentView fluent = new FluentView(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentActions -------------------------------------------------

	@Test
	void actionsWrappingConstructorPreservesInstance() {
		Actions existing = new Actions();
		FluentActions fluent = new FluentActions(existing);
		assertSame(existing, fluent.get());
	}
}
