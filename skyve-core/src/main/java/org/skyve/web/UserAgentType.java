package org.skyve.web;

import org.skyve.impl.util.XMLMetaData;

import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlType;

/**
 * Classifies the HTTP client device type, driving UX/UI selection and responsive layout.
 *
 * <p>The device type is detected from the {@code User-Agent} HTTP header at the start
 * of a session and stored on the session. It determines which UX/UI renderer is selected
 * (e.g. {@code phone} maps to a mobile-optimised view set) and is available in SAIL
 * scripts to target a specific device class.
 *
 * <p>Both {@code phone} and {@code tablet} are considered {@link #isMobile() mobile}
 * for the purposes of layout and scroll behaviour.
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public enum UserAgentType {
	/** Smartphone browser — narrow viewport, touch-first layout. */
	phone(true),
	/** Tablet browser — medium viewport, touch-first layout. */
	tablet(true),
	/** Full desktop browser. */
	desktop(false),
	/** Unrecognised or non-browser agent (e.g. headless, crawler). */
	other(false);
	
	private boolean mobile;
	private UserAgentType(boolean mobile) {
		this.mobile = mobile;
	}
	
	/**
	 * Returns {@code true} if this agent type represents a mobile (touch-primary) device.
	 */
	public boolean isMobile() {
		return mobile;
	}
}

