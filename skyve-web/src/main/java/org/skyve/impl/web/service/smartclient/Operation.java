package org.skyve.impl.web.service.smartclient;

/**
 * Defines SmartClient data-source operations handled by Skyve request processors.
 */
@SuppressWarnings("java:S115") // Suppress "Constant names should comply with a naming convention" as these are not constants but enum values
public enum Operation {
	fetch, add, update, remove;
}
