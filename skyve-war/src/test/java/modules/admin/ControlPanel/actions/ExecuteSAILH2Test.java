package modules.admin.ControlPanel.actions;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.AdditionalAnswers;
import org.skyve.impl.sail.mock.MockFacesContext;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.impl.web.faces.views.FacesView;

import jakarta.el.ELContext;
import jakarta.el.ELResolver;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import modules.admin.ControlPanel.ControlPanelExtension;
import util.AbstractH2TestTruncate;

/**
 * Executes real SAIL with the Control Panel's executor, as its SAIL user (saved and committed, so the data is
 * truncated after each test).
 */
@SuppressWarnings("java:S120") // Module test packages mirror the generated document package.
class ExecuteSAILH2Test extends AbstractH2TestTruncate {
	@Inject
	private ExecuteSAIL executeSAIL;

	private ControlPanelExtension bean;

	@BeforeEach
	void setup() {
		bean = ControlPanelSAILFixture.controlPanel();
	}

	@Test
	void theResultsAreTheGeneratedJavaOnTheResultsTab() throws Exception {
		executeSAIL.execute(bean, new MockWebContext());

		String results = bean.getUnescapedResults();
		assertTrue((results != null) && results.contains("login(null, \"TestUser\", \"pass\\\"word\");"), results);
		assertEquals(Integer.valueOf(2), bean.getTabIndex());
	}

	/**
	 * In a PrimeFaces request (the PrimeFaces Control Panel's), the builders would number the generated components on
	 * from the request page's ids, so the generated ids would match no page: the ids must be the same as outside one.
	 */
	@Test
	void aPrimeFacesRequestsPageDoesNotShiftTheGeneratedIds() throws Exception {
		executeSAIL.execute(bean, new MockWebContext());
		String outsideARequest = bean.getUnescapedResults();

		FacesView page = new FacesView();
		for (int i = 0; i < 500; i++) {
			page.nextId();
		}
		FacesContext request = primeFacesRequest(page);
		String inARequest;
		try {
			CurrentFacesContext.set(request);
			executeSAIL.execute(bean, new MockWebContext());
			inARequest = bean.getUnescapedResults();
			assertSame(request, FacesContext.getCurrentInstance(), "the request's context is put back");
		}
		finally {
			CurrentFacesContext.set(null);
		}
		assertEquals(outsideARequest, inARequest);
	}

	/**
	 * Returns a stand-in for a PrimeFaces request's Faces context (not a MockFacesContext, so the builders treat it as
	 * real) whose page bean, {@code skyve}, is the given view; it makes components as the mock context does.
	 */
	@SuppressWarnings("resource") // the delegate is never installed as the current context, so there is nothing to release
	private static FacesContext primeFacesRequest(FacesView page) {
		ELResolver resolver = mock(ELResolver.class);
		ELContext el = mock(ELContext.class);
		when(el.getELResolver()).thenReturn(resolver);
		when(resolver.getValue(el, null, "skyve")).thenReturn(page);
		FacesContext result = mock(FacesContext.class, AdditionalAnswers.delegatesTo(new MockFacesContext()));
		doReturn(el).when(result).getELContext();
		return result;
	}

	/** Sets the thread's current Faces context, which only a FacesContext subclass may do. */
	private abstract static class CurrentFacesContext extends FacesContext {
		static void set(FacesContext context) {
			setCurrentInstance(context);
		}
	}
}
