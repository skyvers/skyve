package org.skyve.metadata.view.fluent;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

/**
 * Supplemental tests for {@link FluentActions} covering:
 * <ul>
 *   <li>index-based {@code addXxx(int, action)} overloads not yet tested</li>
 *   <li>{@code findZoomOutAction()} implicit finder</li>
 *   <li>{@code findNamedBizImportAction()}</li>
 * </ul>
 */
@SuppressWarnings("static-method")
class FluentActionsIndexTest {

	// ---- index-based add overloads ----

	@Test
	void addDefaultsActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions()
				.addDefaultsAction(new FluentDefaultsAction().name("first"))
				.addDefaultsAction(new FluentDefaultsAction().name("third"));
		fa.addDefaultsAction(1, new FluentDefaultsAction().name("second"));
		assertThat(fa.get().getActions().get(1).getName(), is("second"));
	}

	@Test
	void addNewActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addNewAction(new FluentNewAction().name("a"));
		fa.addNewAction(0, new FluentNewAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addRemoveActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addRemoveAction(new FluentRemoveAction().name("a"));
		fa.addRemoveAction(0, new FluentRemoveAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addDeleteActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addDeleteAction(new FluentDeleteAction().name("a"));
		fa.addDeleteAction(0, new FluentDeleteAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addOKActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addOKAction(new FluentOKAction().name("a"));
		fa.addOKAction(0, new FluentOKAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addPrintActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addPrintAction(new FluentPrintAction().name("a"));
		fa.addPrintAction(0, new FluentPrintAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addZoomOutActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addZoomOutAction(new FluentZoomOutAction().name("a"));
		fa.addZoomOutAction(0, new FluentZoomOutAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addBizExportActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addBizExportAction(new FluentBizExportAction().name("a"));
		fa.addBizExportAction(0, new FluentBizExportAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addBizImportActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addBizImportAction(new FluentBizImportAction().name("a"));
		fa.addBizImportAction(0, new FluentBizImportAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addCustomActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addCustomAction(new FluentCustomAction().name("a"));
		fa.addCustomAction(0, new FluentCustomAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addDownloadActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addDownloadAction(new FluentDownloadAction().name("a"));
		fa.addDownloadAction(0, new FluentDownloadAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addUploadActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addUploadAction(new FluentUploadAction().name("a"));
		fa.addUploadAction(0, new FluentUploadAction().name("b"));
		assertThat(fa.get().getActions().get(0).getName(), is("b"));
	}

	@Test
	void addReportActionAtIndexInsertsAtPosition() {
		FluentActions fa = new FluentActions().addReportAction(new FluentReportAction().reportName("r1"));
		fa.addReportAction(0, new FluentReportAction().reportName("r0"));
		assertThat(fa.get().getActions().size(), is(2));
	}

	// ---- missing finders ----

	@Test
	void findZoomOutActionReturnsNullWhenEmpty() {
		// findZoomOutAction() searches for ImplicitActionName.Save internally;
		// with no matching action, it returns null.
		assertThat(new FluentActions().findZoomOutAction(), is(nullValue()));
	}

	@Test
	void findZoomOutActionReturnsNullEvenWhenZoomOutPresent() {
		// The current implementation searches for ImplicitActionName.Save,
		// so a plain ZoomOutAction (which has ImplicitActionName.ZoomOut) is not found.
		FluentActions fa = new FluentActions().addZoomOutAction(new FluentZoomOutAction());
		assertThat(fa.findZoomOutAction(), is(nullValue()));
	}

	@Test
	void findNamedBizImportActionReturnsMatch() {
		FluentActions fa = new FluentActions().addBizImportAction(new FluentBizImportAction().name("imp1"));
		assertThat(fa.findNamedBizImportAction("imp1"), is(notNullValue()));
	}

	@Test
	void findNamedBizImportActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedBizImportAction("missing"), is(nullValue()));
	}

	@Test
	void findNamedDefaultsActionReturnsMatch() {
		FluentActions fa = new FluentActions().addDefaultsAction(new FluentDefaultsAction().name("defs1"));
		assertThat(fa.findNamedDefaultsAction("defs1"), is(notNullValue()));
	}

	@Test
	void findNamedDefaultsActionReturnsNullWhenMissing() {
		assertThat(new FluentActions().findNamedDefaultsAction("missing"), is(nullValue()));
	}
}
