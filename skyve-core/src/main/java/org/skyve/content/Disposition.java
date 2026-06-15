package org.skyve.content;

/**
 * Controls the HTTP {@code Content-Disposition} header when serving managed content.
 *
 * <p>{@code attachment} instructs the browser to download the content as a file;
 * {@code inline} instructs it to display the content within the browser window if
 * the MIME type is supported (e.g. PDF, image).
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum Disposition {
	/** Content should be downloaded as a file attachment. */
	attachment,
	/** Content should be displayed inline in the browser. */
	inline;
}
