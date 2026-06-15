/**
 * Extended utility API for Skyve applications.
 *
 * <p>Key utilities in this package:
 * <ul>
 *   <li>{@link org.skyve.util.Mail} — builder for email messages (recipients, CC, BCC,
 *       subject, HTML body, attachments).</li>
 *   <li>{@link org.skyve.util.MailService} — service for sending {@link org.skyve.util.Mail}
 *       messages; obtain via {@link org.skyve.EXT#getMailService()}.</li>
 *   <li>{@link org.skyve.util.SMSService} — service for sending SMS messages.</li>
 *   <li>{@link org.skyve.util.PushMessage} — sends server-push (WebSocket/SSE) messages
 *       to one or more active browser sessions.</li>
 *   <li>{@link org.skyve.util.FileUtil} — file I/O helpers (copy, move, delete, zip).</li>
 *   <li>{@link org.skyve.util.SecurityUtil} — password hashing, security log recording, and
 *       security-policy helpers.</li>
 *   <li>{@link org.skyve.util.Thumbnail} — image thumbnail generation.</li>
 *   <li>{@link org.skyve.util.GeoIPService} — IP-address to location lookup.</li>
 *   <li>{@link org.skyve.util.CommunicationUtil} — document-driven communication (sending
 *       templated emails based on {@code admin.Communication} records).</li>
 * </ul>
 *
 * <p>All static utility methods are thread-safe unless otherwise documented.
 */
package org.skyve.util;
