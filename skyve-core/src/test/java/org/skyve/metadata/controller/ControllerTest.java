package org.skyve.metadata.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

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
		assertThat(ImplicitActionName.OK.isValidatable(), is(true));
	}

	@Test
	void saveIsValidatable() {
		assertThat(ImplicitActionName.Save.isValidatable(), is(true));
	}

	@Test
	void cancelIsNotValidatable() {
		assertThat(ImplicitActionName.Cancel.isValidatable(), is(false));
	}

	@Test
	void newIsNotValidatable() {
		assertThat(ImplicitActionName.New.isValidatable(), is(false));
	}

	@Test
	void deleteIsValidatable() {
		assertThat(ImplicitActionName.Delete.isValidatable(), is(true));
	}

	@Test
	void addIsNotValidatable() {
		assertThat(ImplicitActionName.Add.isValidatable(), is(false));
	}

	@Test
	void zoomOutIsValidatable() {
		assertThat(ImplicitActionName.ZoomOut.isValidatable(), is(true));
	}

	@Test
	void removeIsNotValidatable() {
		assertThat(ImplicitActionName.Remove.isValidatable(), is(false));
	}

	@Test
	void editIsValidatable() {
		assertThat(ImplicitActionName.Edit.isValidatable(), is(true));
	}

	@Test
	void reportIsNotValidatable() {
		assertThat(ImplicitActionName.Report.isValidatable(), is(false));
	}

	@Test
	void navigateIsValidatable() {
		assertThat(ImplicitActionName.Navigate.isValidatable(), is(true));
	}

	@Test
	void bizImportIsValidatable() {
		assertThat(ImplicitActionName.BizImport.isValidatable(), is(true));
	}

	@Test
	void bizExportIsValidatable() {
		assertThat(ImplicitActionName.BizExport.isValidatable(), is(true));
	}

	@Test
	void downloadIsValidatable() {
		assertThat(ImplicitActionName.Download.isValidatable(), is(true));
	}

	@Test
	void uploadIsValidatable() {
		assertThat(ImplicitActionName.Upload.isValidatable(), is(true));
	}

	@Test
	void printIsValidatable() {
		assertThat(ImplicitActionName.Print.isValidatable(), is(true));
	}

	@Test
	void defaultsIsNotValidatable() {
		assertThat(ImplicitActionName.DEFAULTS.isValidatable(), is(false));
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
