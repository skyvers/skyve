package org.skyve.domain.app.admin;

import jakarta.annotation.Nullable;

/**
 * Domain contract for reusable communication templates used to generate
 * outbound message subject/body content.
 */
public interface CommunicationTemplate {
	@Nullable String getTemplate();
}
