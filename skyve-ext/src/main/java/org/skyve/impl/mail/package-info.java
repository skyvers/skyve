/**
 * Mail service implementations for Skyve applications.
 *
 * <p>Production implementations include {@code SMTPMailService} (JavaMail SMTP),
 * {@code AzureCommunication} (Azure Communication Services), and
 * {@code Postmark} (Postmark API). Utility / no-op variants include
 * {@code LoggingMailService}, {@code NoOpMailService}, and
 * {@code PreProcessingMailService}. {@code MailServiceStaticSingleton} holds
 * the configured runtime instance.
 */
package org.skyve.impl.mail;
