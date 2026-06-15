package modules.admin.ControlPanel;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class ControlPanelExtensionTest extends AbstractH2Test {

	@Test
	void trapExceptionSetsResultsWithStackTrace() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.trapException(new RuntimeException("test error"));
		assertNotNull(cp.getResults());
		assertTrue(cp.getResults().contains("RuntimeException"));
	}

	@Test
	void setResultsWithEscapingEscapesCurlyBrackets() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setResults("{test}", true);
		assertTrue(cp.getResults().contains("\\{test}"));
	}

	@Test
	void setResultsWithoutEscapingDoesNotEscape() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setResults("{test}", false);
		assertEquals("{test}", cp.getResults());
	}

	@Test
	void setResultsWithNullSetsNullResults() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setResults(null, true);
		assertNull(cp.getResults());
	}

	@Test
	void getUnescapedResultsReturnsOriginalValue() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setResults("{test}", true);
		assertEquals("{test}", cp.getUnescapedResults());
	}

	@Test
	void setBizletTraceSetsUtilImplField() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setBizletTrace(Boolean.TRUE);
		assertEquals(Boolean.TRUE, cp.getBizletTrace());
		cp.setBizletTrace(Boolean.FALSE);
		assertEquals(Boolean.FALSE, cp.getBizletTrace());
	}

	@Test
	void addDocumentToCreateReturnsModuleDocumentWithDocumentName() {
		ControlPanelExtension cp = new ControlPanelExtension();
		cp.setTestModuleName("admin");
		var result = cp.addDocumentToCreate("User");
		assertNotNull(result);
		assertEquals("admin", result.getModuleName());
		assertEquals("User", result.getDocumentName());
	}
}
