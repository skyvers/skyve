/**
 * Test utilities for Skyve applications and framework unit tests.
 *
 * <p>{@link org.skyve.util.test.SkyveFixture} is an annotation applied to factory methods
 * in a {@code *Factory} class to declare test data fixtures. {@link org.skyve.util.test.SkyveFactory}
 * is the corresponding annotation that marks a class as a Skyve document factory.
 *
 * <p>{@link org.skyve.util.test.TestUtil} provides helper methods for bootstrapping
 * Skyve in-process for unit tests (setting up the metadata repository, etc.).
 *
 * @see org.skyve.util.DataBuilder
 */
package org.skyve.util.test;
