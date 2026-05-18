package org.skyve.metadata.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class ControllerTest {

	// ---- ImplicitActionName ----

	@Test
	void implicitActionNameValuesNotNull() {
		assertThat(ImplicitActionName.values(), is(notNullValue()));
	}

	@Test
	void okIsValidatable() {
		assertTrue(ImplicitActionName.OK.isValidatable());
	}

	@Test
	void saveIsValidatable() {
		assertTrue(ImplicitActionName.Save.isValidatable());
	}

	@Test
	void cancelIsNotValidatable() {
		assertFalse(ImplicitActionName.Cancel.isValidatable());
	}

	@Test
	void newIsNotValidatable() {
		assertFalse(ImplicitActionName.New.isValidatable());
	}

	@Test
	void deleteIsValidatable() {
		assertTrue(ImplicitActionName.Delete.isValidatable());
	}

	@Test
	void addIsNotValidatable() {
		assertFalse(ImplicitActionName.Add.isValidatable());
	}

	@Test
	void zoomOutIsValidatable() {
		assertTrue(ImplicitActionName.ZoomOut.isValidatable());
	}

	@Test
	void removeIsNotValidatable() {
		assertFalse(ImplicitActionName.Remove.isValidatable());
	}

	@Test
	void editIsValidatable() {
		assertTrue(ImplicitActionName.Edit.isValidatable());
	}

	@Test
	void reportIsNotValidatable() {
		assertFalse(ImplicitActionName.Report.isValidatable());
	}

	@Test
	void navigateIsValidatable() {
		assertTrue(ImplicitActionName.Navigate.isValidatable());
	}

	@Test
	void bizImportIsValidatable() {
		assertTrue(ImplicitActionName.BizImport.isValidatable());
	}

	@Test
	void bizExportIsValidatable() {
		assertTrue(ImplicitActionName.BizExport.isValidatable());
	}

	@Test
	void downloadIsValidatable() {
		assertTrue(ImplicitActionName.Download.isValidatable());
	}

	@Test
	void uploadIsValidatable() {
		assertTrue(ImplicitActionName.Upload.isValidatable());
	}

	@Test
	void printIsValidatable() {
		assertTrue(ImplicitActionName.Print.isValidatable());
	}

	@Test
	void defaultsIsNotValidatable() {
		assertFalse(ImplicitActionName.DEFAULTS.isValidatable());
	}

	@Test
	void getDisplayNameNotNull() {
		for (ImplicitActionName name : ImplicitActionName.values()) {
			assertThat(name.getDisplayName(), is(notNullValue()));
		}
	}

	@Test
	void valueOfOK() {
		assertThat(ImplicitActionName.valueOf("OK"), is(ImplicitActionName.OK));
	}

	@Test
	void valueOfSave() {
		assertThat(ImplicitActionName.valueOf("Save"), is(ImplicitActionName.Save));
	}
}
