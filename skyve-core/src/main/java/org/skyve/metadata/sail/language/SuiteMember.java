package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.util.Util;

import jakarta.annotation.Nonnull;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlList;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL file that a {@link Suite} runs: a {@code <workflow>} file, or an {@code <automation>} file (a workflow of one).
 * This type is a suite's {@code <setUp>} and {@code <restore>}; an ordinary {@code <member>} is an
 * {@link OrdinarySuiteMember}.
 *
 * <p>{@code file} is relative to the suite file's folder (a classpath resource or a file). It also names the member:
 * the {@code requires} of other members refer to it by this value, exactly as written.
 *
 * <p>{@code requires} names the members (by their {@code file}) that must pass before this one runs. A member whose
 * requirement failed or was skipped is skipped. Set-up and restore members are always locked to their own test
 * strategy; see {@link Suite#isTestStrategyLocked(SuiteMember)}.
 *
 * <p>Not thread-safe: a member is built, or unmarshalled, before a suite runs.
 *
 * @see Suite
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"file", "requires"})
public class SuiteMember {
	private String file;
	private final List<String> requires = new ArrayList<>();

	/**
	 * Returns the workflow or automation file, relative to the suite file's folder.
	 *
	 * @return the file; never {@code null} in a suite that passed {@link Suite#validate(String)}
	 */
	public @Nonnull String getFile() {
		return file;
	}

	/**
	 * Sets the workflow or automation file, relative to the suite file's folder.
	 *
	 * @param file	the file; a blank value is stored as {@code null}, which {@link Suite#validate(String)} rejects
	 */
	@XmlAttribute(name = "file", required = true)
	public void setFile(@Nonnull String file) {
		this.file = Util.processStringValue(file);
	}

	/**
	 * Returns the files of the members that must pass before this member runs (the space-separated {@code requires}
	 * attribute), in the order written. A member also requires set-up members without naming them; see
	 * {@link Suite#requirementsOf(SuiteMember)}.
	 *
	 * @return the required members' files (a live, possibly empty list)
	 */
	@XmlAttribute(name = "requires")
	@XmlList
	public @Nonnull List<String> getRequires() {
		return requires;
	}
}
