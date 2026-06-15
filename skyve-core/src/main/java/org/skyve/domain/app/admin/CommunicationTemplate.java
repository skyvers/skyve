package org.skyve.domain.app.admin;

/**
 * Domain contract for reusable communication templates used to generate
 * outbound message subject/body content.
 */
public interface CommunicationTemplate {
	String getTemplate();
}
