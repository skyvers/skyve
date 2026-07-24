package util.deployed.transport.theme;

import java.io.Serializable;

import org.skyve.impl.web.UserAgent;

import jakarta.faces.context.FacesContext;
import jakarta.faces.view.ViewScoped;
import jakarta.inject.Named;
import jakarta.servlet.http.HttpServletRequest;
import util.deployed.transport.DeployedItEventRecorder;

/** View-scoped fixture used only by {@code ThemeResolutionLifecycleIT}. */
@Named("_deployedThemeLifecycle")
@ViewScoped
public final class ThemeResolutionFixture implements Serializable {
	private static final long serialVersionUID = 1L;
	private static final String CORRELATION_HEADER = "X-Skyve-Deployed-It-Correlation";
	private static final String SELECTION_MARKER_KEY =
			"org.skyve.impl.web.faces.SkyveFacesPhaseListener$FacesViewSelectionMarker";

	private String modelValue;
	private boolean selectionMarkerRemoved;

	public String getModelValue() {
		return modelValue;
	}

	public void setModelValue(String modelValue) {
		this.modelValue = modelValue;
		recordEvent(ThemeResolutionEvent.MODEL_UPDATE);
	}

	public String getUxUiName() {
		return UserAgent.getSelection(request()).getUxUi().getName();
	}

	public String getUserAgentType() {
		return UserAgent.detectType(request()).name();
	}

	public boolean isEmulated() {
		return UserAgent.getSelection(request()).isEmulated();
	}

	public void preRender() {
		if ((! selectionMarkerRemoved) && "true".equals(request().getParameter("removeSelectionMarker"))) {
			Object removedMarker = FacesContext.getCurrentInstance().getViewRoot().getViewMap().remove(SELECTION_MARKER_KEY);
			if (removedMarker == null) {
				throw new IllegalStateException("The view selection marker was not present");
			}
			selectionMarkerRemoved = true;
			recordEvent(ThemeResolutionEvent.SELECTION_MARKER_REMOVED);
		}
	}

	public void normalAction() {
		recordEvent(ThemeResolutionEvent.ACTION_INVOKED);
	}

	public void ajaxAction() {
		recordEvent(ThemeResolutionEvent.ACTION_INVOKED);
	}

	private static HttpServletRequest request() {
		return (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
	}

	private static void recordEvent(ThemeResolutionEvent event) {
		String correlationId = request().getHeader(CORRELATION_HEADER);
		DeployedItEventRecorder.recordEvent(correlationId, event.name());
	}
}
