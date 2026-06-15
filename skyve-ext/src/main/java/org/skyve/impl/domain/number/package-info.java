/**
 * Document number sequence generator using an autonomous (inner) database transaction.
 *
 * <p>{@code DocumentNumberAutonomousTransactionGenerator} extends
 * {@code AbstractDocumentNumberGenerator} to commit sequence number
 * allocations immediately, independent of the calling transaction.
 */
package org.skyve.impl.domain.number;
