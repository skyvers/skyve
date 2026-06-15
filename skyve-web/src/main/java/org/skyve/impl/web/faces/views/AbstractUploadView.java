package org.skyve.impl.web.faces.views;

import java.util.regex.Pattern;

import org.primefaces.model.file.UploadedFile;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.web.AbstractWebContext;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.util.OWASP;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import jakarta.faces.annotation.ManagedProperty;
import jakarta.faces.application.FacesMessage;
import jakarta.faces.context.FacesContext;
import jakarta.inject.Inject;
import jakarta.servlet.http.HttpSession;

/**
 * Provides common upload view state and validation helpers for JSF-backed upload endpoints.
 *
 * <p>Threading: JSF view scoped; instances are request/UI thread confined.
 */
public abstract class AbstractUploadView extends LocalisableView {
	private static final long serialVersionUID = 8618349823087627588L;

	/**
	 * Number of bytes in one megabyte.
	 */
	public static final long MB_IN_BYTES = 1024L * 1024L;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.CONTEXT_NAME + "}")
	@SuppressWarnings("java:S6813") // allow member injection
	private String context;

	@Inject
	@ManagedProperty(value = "#{param." + AbstractWebContext.BINDING_NAME + "}")
	@SuppressWarnings("java:S6813") // allow member injection
	private String binding;

	// Indicates that there is a session and a skyve user in that session.
	// Used to render a banner with login link (to replace winodw.location in frame bust)
	private boolean canAccess;

	/**
	 * Case-insensitive whitelist regex applied to uploaded file names.
	 */
	private String whitelistRegex;
	
	/**
	 * Maximum accepted upload size in bytes.
	 */
	private long maximumSizeInBytes;

	/**
	 * Creates a base upload view with explicit filename whitelist and size constraints.
	 *
	 * @param whitelistRegex case-insensitive regex used to validate file names
	 * @param maximumSizeMB maximum allowed upload size in megabytes
	 */
	protected AbstractUploadView(String whitelistRegex, int maximumSizeMB) {
		this.whitelistRegex = whitelistRegex;
		maximumSizeInBytes = maximumSizeMB * MB_IN_BYTES;
	}

	/**
	 * Initialises sanitised request parameters and resolves whether an authenticated session is present.
	 */
	// Call this from extending views
	protected void postConstruct() {
		context = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(context));
		binding = OWASP.sanitise(Sanitisation.text, UtilImpl.processStringValue(binding));
		HttpSession session = (HttpSession) FacesContext.getCurrentInstance().getExternalContext().getSession(false);
		canAccess = (session != null) && (session.getAttribute(WebContext.USER_SESSION_ATTRIBUTE_NAME) != null);
	}
	
	/**
	 * Returns the sanitised upload context identifier.
	 *
	 * @return the upload context identifier
	 */
	public String getContext() {
		return context;
	}

	/**
	 * Returns the sanitised binding expression targeted by this upload view.
	 *
	 * @return the binding expression
	 */
	public String getBinding() {
		return binding;
	}

	/**
	 * Indicates whether the current HTTP session has a Skyve user and can access this upload view.
	 *
	 * @return {@code true} when a valid Skyve user session exists
	 */
	public boolean isCanAccess() {
		return canAccess;
	}

	/**
	 * Returns the application base URL used by upload pages for frame-safe redirection links.
	 *
	 * @return the application base URL
	 */
	@SuppressWarnings("static-method")
	public final String getBaseHref() {
		return Util.getBaseUrl();
	}

	/**
	 * Returns the configured case-insensitive whitelist regex for file names.
	 *
	 * @return the whitelist regex, or {@code null} when unrestricted by regex
	 */
	public String getWhitelistRegex() {
		return whitelistRegex;
	}

	/**
	 * Returns the configured maximum upload size in bytes.
	 *
	 * @return maximum upload size in bytes
	 */
	public long getMaximumSizeInBytes() {
		return maximumSizeInBytes;
	}
	
	/**
	 * Validates upload size and filename safety constraints.
	 *
	 * @param file the uploaded file metadata and stream wrapper
	 * @param fc the active faces context used to enqueue validation messages
	 * @return {@code true} when the file passes configured size and whitelist checks
	 */
	protected boolean validFile(UploadedFile file, FacesContext fc) {
		long size = file.getSize();
		if (size > maximumSizeInBytes) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("FileUpload - File size of {} > maximumSizeInBytes of {}", Long.toString(size), Long.toString(maximumSizeInBytes));
			}
			FacesMessage msg = new FacesMessage("Failure", "File is too large");
			fc.addMessage(null, msg);
			return false;
		}

		String name = file.getFileName();
		// NB PatternSyntaxException is caught in SkyveContextListener at startup
		if ((name != null) &&
				(((whitelistRegex != null) &&
				// Check whitelist regex if defined
				(! Pattern.compile(whitelistRegex, Pattern.CASE_INSENSITIVE | Pattern.MULTILINE).matcher(name).matches())) ||
				// Always disallow files starting with . or any path segment starting with .
				name.startsWith(".") || name.contains("/.") || name.contains("\\."))) {
			LOGGER.warn("FileUpload - Filename {} does not match {}", name, whitelistRegex);
			FacesMessage msg = new FacesMessage("Failure", "Filename " + name + " is not allowed");
			fc.addMessage(null, msg);
			return false;
		}
		
		return true;
	}
}

