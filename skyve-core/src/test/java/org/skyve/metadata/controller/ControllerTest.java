package org.skyve.metadata.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.bizport.BizPortWorkbook;
import org.skyve.domain.Bean;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.user.Role;
import org.skyve.web.WebContext;

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

	// ---- abstract action constructors ----

	@Test
	void bizExportActionCanBeInstantiated() {
		BizExportAction action = new BizExportAction() {
			@Override
			public BizPortWorkbook bizExport(WebContext webContext) {
				return null;
			}
		};
		assertNotNull(action);
	}

	@Test
	void bizImportActionCanBeInstantiated() {
		BizImportAction action = new BizImportAction() {
			@Override
			public void bizImport(BizPortWorkbook bizPortable, UploadException problems) {
				// no-op
			}
		};
		assertNotNull(action);
	}

	@Test
	void downloadActionCanBeInstantiated() {
		DownloadAction<Bean> action = new DownloadAction<>() {
			@Override
			public void prepare(Bean bean, WebContext webContext) {
				// no-op
			}
			@Override
			public Download download(Bean bean, WebContext webContext) {
				return null;
			}
		};
		assertNotNull(action);
	}

	@Test
	void uploadActionCanBeInstantiated() {
		UploadAction<Bean> action = new UploadAction<>() {
			@Override
			public Bean upload(Bean bean, Upload upload, UploadException exception, WebContext webContext) {
				return null;
			}
		};
		assertNotNull(action);
	}

	// ---- Role.getLocalisedDescription ----

	@Test
	void roleGetLocalisedDescriptionWithNullDescriptionReturnsNull() {
		Role role = new Role() {
			@Override
			public String getName() {
				return "TestRole";
			}
			@Override
			public Map<String, String> getProperties() {
				return null;
			}
			@Override
			public String getDescription() {
				return null;
			}
			@Override
			public org.skyve.metadata.module.Module getOwningModule() {
				return null;
			}
			@Override
			public String getDocumentation() {
				return null;
			}
		};
		assertNull(role.getLocalisedDescription());
	}
}
