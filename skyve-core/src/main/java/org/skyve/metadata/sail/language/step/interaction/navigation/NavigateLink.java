package org.skyve.metadata.sail.language.step.interaction.navigation;

import org.skyve.impl.sail.execution.AutomationContext;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.execution.Executor;
import org.skyve.metadata.sail.language.Step;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Navigates the browser by a link, without relying on a module/document binding: either to the URL or Skyve view
 * that {@code href} names, or by clicking the first displayed link ({@code <a>}) on the page whose visible text
 * contains {@code text} (case-sensitive), such as a link in a blurb's markup. With {@code exact="true"}, the link's visible
 * text, trimmed, must equal {@code text}.
 *
 * <p>Invariant: exactly one of {@link #getHref()} and {@link #getText()} is set on a step that is about to be
 * executed. The XSD cannot express this, so {@link #execute(Executor)} checks it.
 *
 * <p>Executors pass both through {@code ContextualExecutor.substituteTokens(String)}, so that {@code {run}} and
 * {@code {var:name}} resolve.
 *
 * @see NavigateList
 * @see NavigateEdit
 * @see org.skyve.metadata.sail.execution.Executor#executeNavigateLink
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class NavigateLink implements Step {

	private String href;
	private String text;
	private Boolean exact; // null/false: the link's text contains the text; true: equals it
	
	/**
	 * Returns the URL to open: absolute, or relative to the application's base URL.
	 * @return the URL, or {@code null} when the step clicks a link by its {@link #getText()}
	 */
	public @Nullable String getHref() {
		return href;
	}

	/**
	 * Sets the href.
	 * @param href the href
	 */
	@XmlAttribute(name = "href")
	public void setHref(@Nullable String href) {
		this.href = UtilImpl.processStringValue(href);
	}

	/**
	 * Returns the text that the visible text of the link to click contains (case-sensitive).
	 * @return the text, or {@code null} when the step opens its {@link #getHref()}
	 */
	public @Nullable String getText() {
		return text;
	}

	/**
	 * Sets the text.
	 * @param text the text
	 */
	@XmlAttribute(name = "text")
	public void setText(@Nullable String text) {
		this.text = UtilImpl.processStringValue(text);
	}

	/**
	 * Returns whether the link's visible text must equal {@link #getText()} rather than contain it.
	 * @return {@code true} to match the whole text; {@code false} or {@code null} to match text it contains
	 */
	public @Nullable Boolean getExact() {
		return exact;
	}

	/**
	 * Sets whether the link's visible text, trimmed, must equal {@link #getText()} (case-sensitive), rather than contain it.
	 * Only applies with {@code text}.
	 * @param exact {@code true} to match the whole text; {@code false} or {@code null} to match text it contains
	 */
	@XmlAttribute(name = "exact")
	public void setExact(@Nullable Boolean exact) {
		this.exact = exact;
	}

	/**
	 * Checks that exactly one of {@code href} and {@code text} is set, then executes the step.
	 * @param executor the executor
	 * @throws MetaDataException if neither or both of {@code href} and {@code text} are set, or {@code exact} is set
	 * 								with {@code href}
	 */
	@Override
	public void execute(@Nonnull Executor executor) {
		if ((href == null) == (text == null)) {
			throw new MetaDataException("<NavigateLink /> requires exactly one of [href] or [text]");
		}
		if ((exact != null) && (text == null)) {
			throw new MetaDataException("<NavigateLink /> [exact] applies only to [text]");
		}
		executor.executeNavigateLink(this);
	}

	/**
	 * Returns the identifier: the href, or else the text.
	 * @param context the context
	 * @return the href or text, or an empty string if neither is set
	 */
	@Override
	public @Nonnull String getIdentifier(@Nonnull AutomationContext context) {
		String result = (href != null) ? href : text;
		return (result == null) ? "" : result;
	}
}
