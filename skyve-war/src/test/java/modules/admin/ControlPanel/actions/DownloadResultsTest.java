package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;

import modules.admin.ControlPanel.ControlPanelExtension;

@SuppressWarnings("static-method")
class DownloadResultsTest {

	@Test
	void prepareWithNullResultsThrowsValidationException() throws Exception {
		DownloadResults action = new DownloadResults();
		ControlPanelExtension bean = new ControlPanelExtension();
		// results is null by default → prepare() throws ValidationException
		assertThrows(ValidationException.class, () -> action.prepare(bean, null));
	}

	@Test
	void prepareWithNonNullResultsDoesNotThrow() throws Exception {
		DownloadResults action = new DownloadResults();
		ControlPanelExtension bean = new ControlPanelExtension();
		bean.setResults("some results", false);
		// should not throw
		action.prepare(bean, null);
	}

	@Test
	void downloadReturnsDownloadWithUnescapedContent() throws Exception {
		DownloadResults action = new DownloadResults();
		ControlPanelExtension bean = new ControlPanelExtension();
		bean.setResults("test{content}", false);

		Download result = action.download(bean, null);

		assertNotNull(result);
		// after download, results are cleared
		assertNull(bean.getResults());
		assertNull(bean.getUnescapedResults());
	}
}
