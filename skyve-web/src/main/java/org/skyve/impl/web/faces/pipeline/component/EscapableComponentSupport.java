package org.skyve.impl.web.faces.pipeline.component;

import org.primefaces.behavior.confirm.ConfirmBehavior;
import org.primefaces.component.outputlabel.OutputLabel;

import jakarta.faces.application.Application;
import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UIComponentBase;
import jakarta.faces.component.html.HtmlOutputText;

/**
 * Creates PrimeFaces/JSF components that keep metadata text raw until the
 * component renderer applies the paired escape decision.
 */
final class EscapableComponentSupport {
	/**
	 * Prevents utility construction.
	 */
	private EscapableComponentSupport() {
		// utility class
	}

	/**
	 * Creates an {@code h:outputText} carrying raw metadata text and an escape flag.
	 *
	 * <p>Side effects: asks {@code application} to create the component. The returned
	 * component is detached from any JSF component tree.
	 *
	 * @param application JSF application used to create the component; must not be {@code null}
	 * @param text raw metadata text and nullable escape flag; must not be {@code null}
	 * @return configured output-text component; never {@code null}
	 */
	static HtmlOutputText outputText(Application application, EscapableText text) {
		HtmlOutputText result = (HtmlOutputText) application.createComponent(HtmlOutputText.COMPONENT_TYPE);
		result.setValue(text.getValue());
		result.setEscape(text.shouldEscape());
		return result;
	}

	/**
	 * Creates a PrimeFaces output label carrying raw metadata text and an escape flag.
	 *
	 * <p>Side effects: asks {@code application} to create the component. The returned
	 * component is detached from any JSF component tree.
	 *
	 * @param application JSF application used to create the component; must not be {@code null}
	 * @param text raw metadata label and nullable escape flag; must not be {@code null}
	 * @param forId optional ID of the labelled component
	 * @return configured output-label component; never {@code null}
	 */
	static OutputLabel outputLabel(Application application, EscapableText text, String forId) {
		OutputLabel result = (OutputLabel) application.createComponent(OutputLabel.COMPONENT_TYPE);
		result.setValue(text.getValue());
		result.setEscape(text.shouldEscape());
		result.setFor(forId);
		return result;
	}

	/**
	 * Adds an output-text facet when metadata text is present.
	 *
	 * <p>Side effects: mutates {@code component.getFacets()} by adding or replacing
	 * {@code facetName}. No facet is added when {@code text} or its value is {@code null}.
	 *
	 * @param application JSF application used to create the facet component; must not be {@code null}
	 * @param component component receiving the facet; must not be {@code null}
	 * @param facetName facet key to add or replace; must not be {@code null}
	 * @param text raw metadata text and nullable escape flag; may be {@code null}
	 * @return the added output-text facet, or {@code null} when no text value is present
	 */
	static HtmlOutputText putOutputTextFacet(Application application, UIComponent component, String facetName, EscapableText text) {
		if ((text == null) || (text.getValue() == null)) {
			return null;
		}

		HtmlOutputText facet = outputText(application, text);
		component.getFacets().put(facetName, facet);
		return facet;
	}

	/**
	 * Creates a PrimeFaces confirmation behaviour carrying raw text and an escape flag.
	 *
	 * <p>Side effects: asks {@code application} to create the behaviour. The returned
	 * behaviour is detached from any JSF component tree.
	 *
	 * @param application JSF application used to create the behaviour; must not be {@code null}
	 * @param confirmation raw confirmation text and nullable escape flag; may be {@code null}
	 * @return configured confirmation behaviour, or {@code null} when no message value is present
	 */
	static ConfirmBehavior confirmBehavior(Application application, EscapableText confirmation) {
		if ((confirmation == null) || (confirmation.getValue() == null)) {
			return null;
		}

		ConfirmBehavior result = (ConfirmBehavior) application.createBehavior(ConfirmBehavior.BEHAVIOR_ID);
		result.setMessage(confirmation.getValue());
		result.setEscape(confirmation.shouldEscape());
		return result;
	}

	/**
	 * Adds a click confirmation behaviour when metadata confirmation text is present.
	 *
	 * <p>Side effects: mutates {@code component} by attaching a PrimeFaces client
	 * behaviour for the {@code click} event. No behaviour is added when
	 * {@code confirmation} or its value is {@code null}.
	 *
	 * @param application JSF application used to create the behaviour; must not be {@code null}
	 * @param component component receiving the behaviour; must not be {@code null}
	 * @param confirmation raw confirmation text and nullable escape flag; may be {@code null}
	 */
	static void addConfirmBehavior(Application application, UIComponentBase component, EscapableText confirmation) {
		ConfirmBehavior behavior = confirmBehavior(application, confirmation);
		if (behavior != null) {
			component.addClientBehavior("click", behavior);
		}
	}
}
