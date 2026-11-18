package sail.admin;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import util.AbstractInterpretedSailWorkflow;

/**
 * Runs the admin module suite, {@code sail/admin/admin.suite.xml}, across both renderers: a backup, marker and restore
 * bracket around independent members for PrimeFaces ({@code pf/}, tablet and phone) and SmartClient ({@code sc/}). See
 * the suite file for its bracket and order.
 * <p>
 * It backs up, changes, truncates and restores the data: run it only against a disposable server. Runs against
 * {@code -Dsail.baseUrl} (required); {@code -Dsail.suite.only=pf/} (or {@code sc/}, or {@code 03}) runs only those
 * members, with the bracket.
 */
@Tag("sail")
@Tag("admin")
@Tag("destructive")
class AdminSuiteSailIT extends AbstractInterpretedSailWorkflow {
	@Test
	@SuppressWarnings("java:S2699") // the suite's test steps are the assertions: the harness fails the test when a member fails
	void suite() {
		sailSuite("sail/admin/admin.suite.xml");
	}
}
