package org.skyve.metadata.module;

import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.util.Util;

/**
 * Metadata descriptor for a background job declared in a module.
 *
 * <p>Jobs are declared in the module XML under {@code <jobs>}. Each entry
 * provides a display name, description, and the fully-qualified class name of the
 * Quartz {@code Job} implementation. The Skyve scheduler uses this metadata to
 * present the job catalogue in the admin UI and to instantiate jobs at runtime.
 *
 * @see org.skyve.metadata.module.Module#getJob(String)
 * @see org.skyve.metadata.module.Module#getJobs()
 */
public interface JobMetaData extends NamedMetaData, DecoratedMetaData {
	/**
	 * Returns the i18n resource key (or literal string) for the human-readable job name.
	 *
	 * @return the display name key; never {@code null}
	 * @see #getLocalisedDisplayName()
	 */
	public String getDisplayName();

	/**
	 * Returns the localised human-readable job name, resolved via the i18n resource bundle.
	 *
	 * @return the localised display name; never {@code null}
	 */
	public default String getLocalisedDisplayName() {
		return Util.i18n(getDisplayName());
	}

	/**
	 * Returns the fully-qualified class name of the job implementation.
	 *
	 * <p>The class must implement {@code org.quartz.Job}. Skyve instantiates it
	 * via the application class loader at schedule time.
	 *
	 * @return the class name; never {@code null}
	 */
	public String getClassName();

	/**
	 * Returns the i18n resource key (or literal string) for a longer description of this job.
	 *
	 * @return the description key; may be {@code null}
	 * @see #getLocalisedDescription()
	 */
	public String getDescription();

	/**
	 * Returns the localised description, resolved via the i18n resource bundle.
	 *
	 * @return the localised description; may be {@code null} if no description is defined
	 */
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}

	/**
	 * Returns the name of the module that declares this job.
	 *
	 * <p>This is a derived property computed from the enclosing module's name.
	 *
	 * @return the owning module name; never {@code null}
	 */
	public String getOwningModuleName();
}
