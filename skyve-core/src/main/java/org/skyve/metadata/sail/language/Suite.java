package org.skyve.metadata.sail.language;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.sail.language.Automation.TestStrategy;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import jakarta.xml.bind.annotation.XmlAttribute;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlRootElement;
import jakarta.xml.bind.annotation.XmlTransient;
import jakarta.xml.bind.annotation.XmlType;

/**
 * A SAIL suite: an execution plan of independent members, each a {@link Workflow} file (an {@link Automation} file is
 * a workflow of one), for example
 * <pre>
 * &lt;suite xmlns="http://www.skyve.org/xml/sail" testStrategy="Assert" evidence="video"&gt;
 * 	&lt;viewRoles user="sail{run}"&gt;
 * 		&lt;role&gt;admin.BasicUser&lt;/role&gt;
 * 	&lt;/viewRoles&gt;
 * 	&lt;setUp file="00-precondition.xml"/&gt;
 * 	&lt;setUp file="00-backup.xml"/&gt;
 * 	&lt;member file="pf/01-session.xml"/&gt;
 * 	&lt;member file="sc/01-session.xml"/&gt;
 * 	&lt;member file="sc/04-devops.xml" requires="pf/03-security-admin.xml" lockTestStrategy="true"/&gt;
 * 	&lt;restore file="99-restore.xml" requires="00-precondition.xml 00-backup.xml"/&gt;
 * &lt;/suite&gt;
 * </pre>
 *
 * <p>A suite runs its set-up members in order, then its ordinary members in order, then its optional restore. Each
 * member gets its own run: members share no {@code {run}} token and no {@code {var:name}} values. A member runs only if
 * every member it requires passed; without naming them, each set-up member requires every earlier set-up member, and
 * every ordinary member requires every set-up member (see {@link #requirementsOf(SuiteMember)}). A failed ordinary
 * member does not stop the others. The restore runs last, even after failures, if its own requirements passed.
 *
 * <p>Set-up members, the restore and ordinary members with {@code lockTestStrategy="true"} are locked to their own test
 * strategy (see {@link #isTestStrategyLocked(SuiteMember)}). {@code testStrategy} is a default for the automations of
 * every unlocked member that states none; an automation's own attribute, then its workflow's, wins (see
 * {@link #getEffectiveTestStrategy(SuiteMember, Workflow, Automation)}). {@code evidence}
 * is a request for the run's evidence, which the run can override. {@code viewRoles} register the roles that the
 * runner builds views with for the users the members sign in as.
 *
 * <p>Like a workflow, a suite is not an {@link Executable}: a runner such as the interpreted harness's
 * {@code util.sail.SailSuite} resolves each member's file relative to the suite file's folder and runs it. Unmarshal one
 * with {@link XMLMetaData#unmarshalSAILSuiteFile(String)}, which also calls {@link #validate(String)}.
 *
 * <p>Not thread-safe: a suite is built, or unmarshalled, before it runs.
 *
 * @see SuiteMember
 * @see OrdinarySuiteMember
 * @see ViewRoles
 */
@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE, propOrder = {"testStrategy", "evidence", "viewRoles", "setUps", "members", "restore"})
@XmlRootElement(namespace = XMLMetaData.SAIL_NAMESPACE)
public class Suite {
	/**
	 * The evidence that a suite asks its run to keep. A run can override the request.
	 */
	@XmlType(namespace = XMLMetaData.SAIL_NAMESPACE)
	@SuppressWarnings("java:S115") // Enum names are SAIL XML values.
	public enum Evidence {
		/** Keep no evidence. */
		none,
		/** Keep a failure's screenshot and page source. */
		failures,
		/** Keep the failures, and a snapshot of every step. */
		snapshots,
		/** Keep the failures, and a video of every automation. */
		video,
		/** Keep the failures, the snapshots and the videos. */
		all
	}

	/** A drive letter, as in {@code C:}, which starts an absolute Windows path. */
	private static final Pattern ABSOLUTE_WINDOWS_PATH = Pattern.compile("[A-Za-z]:");
	/** White space, which would split a file in a {@code requires} list. */
	private static final Pattern WHITE_SPACE = Pattern.compile("\\s");

	private TestStrategy testStrategy;
	private Evidence evidence;
	private final List<ViewRoles> viewRoles = new ArrayList<>();
	private final List<SuiteMember> setUps = new ArrayList<>();
	private final List<OrdinarySuiteMember> members = new ArrayList<>();
	private SuiteMember restore;

	/**
	 * Returns the default test strategy for the automations of every member that states none.
	 *
	 * @return the default, or {@code null} if the suite states none
	 */
	public @Nullable TestStrategy getTestStrategy() {
		return testStrategy;
	}

	/**
	 * Sets the default test strategy for the automations of every member that states none.
	 *
	 * @param testStrategy	the default, or {@code null} for none
	 */
	@XmlAttribute(name = "testStrategy")
	public void setTestStrategy(@Nullable TestStrategy testStrategy) {
		this.testStrategy = testStrategy;
	}

	/**
	 * Returns the evidence that the suite asks its run to keep.
	 *
	 * @return the request, or {@code null} if the suite makes none (the run decides)
	 */
	public @Nullable Evidence getEvidence() {
		return evidence;
	}

	/**
	 * Sets the evidence that the suite asks its run to keep.
	 *
	 * @param evidence	the request, or {@code null} to leave it to the run
	 */
	@XmlAttribute(name = "evidence")
	public void setEvidence(@Nullable Evidence evidence) {
		this.evidence = evidence;
	}

	/**
	 * Returns the roles that the runner builds views with, one entry per user.
	 *
	 * @return the view roles, in the order written (a live, possibly empty list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "viewRoles")
	public @Nonnull List<ViewRoles> getViewRoles() {
		return viewRoles;
	}

	/**
	 * Returns the set-up members, which run first, in order, locked to their own test strategy.
	 *
	 * @return the set-up members (a live, possibly empty list)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "setUp")
	public @Nonnull List<SuiteMember> getSetUps() {
		return setUps;
	}

	/**
	 * Returns the ordinary members, which run in order after the set-up members.
	 *
	 * @return the ordinary members (a live list; at least one in a valid suite)
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "member", required = true)
	public @Nonnull List<OrdinarySuiteMember> getMembers() {
		return members;
	}

	/**
	 * Returns the restore member, which runs last, even after failures, if its own requirements passed. It is locked
	 * to its own test strategy.
	 *
	 * @return the restore member, or {@code null} if there is none
	 */
	public @Nullable SuiteMember getRestore() {
		return restore;
	}

	/**
	 * Sets the restore member.
	 *
	 * @param restore	the restore member, or {@code null} for none
	 */
	@XmlElement(namespace = XMLMetaData.SAIL_NAMESPACE, name = "restore")
	public void setRestore(@Nullable SuiteMember restore) {
		this.restore = restore;
	}

	/**
	 * Returns every member in run order: the set-up members, the ordinary members, then the restore.
	 *
	 * @return a new list
	 */
	@XmlTransient
	public @Nonnull List<SuiteMember> getAllMembers() {
		List<SuiteMember> result = new ArrayList<>(setUps.size() + members.size() + 1);
		result.addAll(setUps);
		result.addAll(members);
		if (restore != null) {
			result.add(restore);
		}
		return result;
	}

	/**
	 * Returns the member with a file, as other members' {@code requires} name it.
	 *
	 * <p>Complexity: O(m) time, where m is the number of members.
	 *
	 * @param file	the member's file, exactly as written
	 * @return the set-up, ordinary or restore member, or {@code null} if no member has the file
	 */
	public @Nullable SuiteMember getMember(@Nonnull String file) {
		for (SuiteMember member : getAllMembers()) {
			if (file.equals(member.getFile())) {
				return member;
			}
		}
		return null;
	}

	/**
	 * Indicates if a member is one of this suite's set-up members.
	 *
	 * @param member	the member
	 * @return {@code true} if it is a set-up member (by identity)
	 */
	public boolean isSetUp(@Nonnull SuiteMember member) {
		return indexOf(setUps, member) >= 0;
	}

	/**
	 * Indicates if a run's test strategy override, and this suite's {@code testStrategy} default, must not apply to a
	 * member. The member's place in the suite decides, not its Java class: a set-up member and the restore are always
	 * locked; an ordinary member is locked if its {@code lockTestStrategy} is {@code true}.
	 *
	 * @param member	the member
	 * @return {@code true} if it is locked; {@code false} if it is an unlocked ordinary member or not in this suite
	 */
	public boolean isTestStrategyLocked(@Nonnull SuiteMember member) {
		if ((member == restore) || isSetUp(member)) {
			return true;
		}
		int index = indexOf(members, member);
		return (index >= 0) && Boolean.TRUE.equals(members.get(index).getLockTestStrategy());
	}

	/**
	 * Returns the members that must pass before a member runs: those its {@code requires} names and, without naming
	 * them, every earlier set-up member for a set-up member, or every set-up member for an ordinary member. A runner
	 * skips the member if any of them failed or was skipped.
	 *
	 * <p>Precondition: the suite is valid ({@link #validate(String)}).
	 *
	 * @param member	a member of this suite; for a member not in it, only the members its {@code requires} names
	 * @return the required members in run order, each once (a new list; empty if there are none)
	 */
	public @Nonnull List<SuiteMember> requirementsOf(@Nonnull SuiteMember member) {
		// the set-ups before this many are required implicitly: the earlier ones for a set-up, all for an ordinary member
		int implicitSetUps = indexOf(setUps, member);
		if (implicitSetUps < 0) {
			implicitSetUps = (indexOf(members, member) >= 0) ? setUps.size() : 0;
		}
		Set<String> required = new HashSet<>(member.getRequires());
		List<SuiteMember> result = new ArrayList<>();
		List<SuiteMember> all = getAllMembers();
		for (int i = 0, l = all.size(); i < l; i++) {
			SuiteMember candidate = all.get(i);
			if ((i < implicitSetUps) || required.contains(candidate.getFile())) {
				result.add(candidate);
			}
		}
		return result;
	}

	/**
	 * Returns the test strategy that an automation of one of this suite's members runs with before any run override:
	 * the automation's own, else its workflow's default; then, for an unlocked member, this suite's default, or for a
	 * locked member, {@link TestStrategy#Assert}. A locked member therefore never resolves to {@code null}, so a run's
	 * override cannot reach it as a missing default.
	 *
	 * @param member	the member whose workflow it is
	 * @param workflow	the member's workflow
	 * @param automation	one of the workflow's automations
	 * @return the strategy; {@code null} only for an unlocked member when none of them states one (the run's
	 * 			override or the runner's default applies)
	 * @see Workflow#getEffectiveTestStrategy(Automation)
	 * @see #isTestStrategyLocked(SuiteMember)
	 */
	public @Nullable TestStrategy getEffectiveTestStrategy(@Nonnull SuiteMember member,
															@Nonnull Workflow workflow,
															@Nonnull Automation automation) {
		TestStrategy result = workflow.getEffectiveTestStrategy(automation);
		if (result == null) {
			result = isTestStrategyLocked(member) ? TestStrategy.Assert : testStrategy;
		}
		return result;
	}

	/**
	 * Checks what the schema cannot: that there is at least one ordinary member; that every member has a normalised
	 * relative file, and no file is listed twice; that every {@code requires} names, once, another member of the suite
	 * that runs before it; that the restore requires only set-up members; and that each view roles entry has a user,
	 * given once, and no blank role. Running before is what rules out a cycle of requirements.
	 *
	 * <p>Complexity: O(m + r) time, where m is the number of members and r the number of requirements.
	 *
	 * @param name	the suite's name for messages, usually its file or resource
	 * @throws MetaDataException	naming the suite and the member at fault, at the first problem found
	 */
	public void validate(@Nonnull String name) {
		String prefix = "SAIL suite " + name + ": ";
		validateViewRoles(prefix);
		if (members.isEmpty()) {
			throw new MetaDataException(prefix + "there is no <member>");
		}

		List<SuiteMember> all = getAllMembers();
		Map<String, Integer> positions = positions(prefix, all);
		for (int i = 0, l = all.size(); i < l; i++) {
			validateRequires(prefix, all, positions, i);
		}
	}

	/**
	 * Throws if a view roles entry has no user, repeats a user, or has no role or a blank one.
	 */
	private void validateViewRoles(@Nonnull String prefix) {
		Set<String> users = new HashSet<>();
		for (ViewRoles entry : viewRoles) {
			String user = entry.getUser();
			@SuppressWarnings({"null", "java:S2583"}) // getUser() is @Nonnull for a valid suite; JAXB and its setter store a blank user as null before validate
			boolean noUser = (user == null);
			if (noUser) {
				throw new MetaDataException(prefix + "a <viewRoles> has no user");
			}
			if (! users.add(user)) {
				throw new MetaDataException(prefix + "<viewRoles> for user " + user + " is given more than once");
			}
			List<String> roles = entry.getRoles();
			if (roles.isEmpty()) {
				throw new MetaDataException(prefix + "<viewRoles> for user " + user + " has no <role>");
			}
			for (String role : roles) {
				if ((role == null) || role.isBlank()) {
					throw new MetaDataException(prefix + "<viewRoles> for user " + user + " has a blank <role>");
				}
			}
		}
	}

	/**
	 * Returns the run order position of each member's file, throwing if a file is missing, not a normalised relative
	 * path, or listed twice.
	 */
	private @Nonnull Map<String, Integer> positions(@Nonnull String prefix, @Nonnull List<SuiteMember> all) {
		Map<String, Integer> result = new HashMap<>();
		for (int i = 0, l = all.size(); i < l; i++) {
			String file = all.get(i).getFile();
			@SuppressWarnings({"null", "java:S2583"}) // getFile() is @Nonnull for a valid suite; JAXB and its setter store a blank file as null before validate
			boolean noFile = (file == null);
			if (noFile) {
				throw new MetaDataException(prefix + "a <" + element(i) + "> has no file");
			}
			String problem = pathProblem(file);
			if (problem != null) {
				throw new MetaDataException(prefix + "<" + element(i) + "> file " + file + " " + problem);
			}
			if (result.put(file, Integer.valueOf(i)) != null) {
				throw new MetaDataException(prefix + file + " is listed more than once");
			}
		}
		return result;
	}

	/**
	 * Returns what is wrong with a member's file as a path relative to the suite file's folder.
	 *
	 * @return the problem, for a message, or {@code null} if it is a normalised relative path
	 */
	private static @Nullable String pathProblem(@Nonnull String file) {
		if (WHITE_SPACE.matcher(file).find()) {
			return "has white space; requires separates files with spaces";
		}
		if (file.indexOf('\\') >= 0) {
			return "has a \\; separate folders with /";
		}
		if (file.startsWith("/") || ABSOLUTE_WINDOWS_PATH.matcher(file).lookingAt()) {
			return "is absolute; it must be relative to the suite's folder";
		}
		for (String segment : file.split("/", -1)) {
			if (segment.isEmpty() || ".".equals(segment) || "..".equals(segment)) {
				return "has an empty, . or .. segment; it must be a normalised path within the suite's folder";
			}
		}
		return null;
	}

	/**
	 * Throws if the member at a run order position requires a file that is not in the suite, itself, a file twice, a
	 * member that runs after it, or (for the restore) a member that is not a set-up member.
	 */
	private void validateRequires(@Nonnull String prefix,
									@Nonnull List<SuiteMember> all,
									@Nonnull Map<String, Integer> positions,
									int index) {
		SuiteMember member = all.get(index);
		Set<String> seen = new HashSet<>();
		for (String required : member.getRequires()) {
			Integer position = positions.get(required);
			if (position == null) {
				throw new MetaDataException(prefix + describe(index, member) + " requires " + required +
												", which is not a member of the suite");
			}
			int p = position.intValue();
			if (p == index) {
				throw new MetaDataException(prefix + describe(index, member) + " requires itself");
			}
			if (! seen.add(required)) {
				throw new MetaDataException(prefix + describe(index, member) + " requires " + required + " more than once");
			}
			if ((member == restore) && (p >= setUps.size())) {
				throw new MetaDataException(prefix + describe(index, member) + " requires " + required +
												", which is not a set-up member; a restore can require only set-up members");
			}
			if (p > index) {
				throw new MetaDataException(prefix + describe(index, member) + " requires " + required + ", which runs after it (" +
												describe(p, all.get(p)) + "); members run in order");
			}
		}
	}

	/**
	 * Returns the element name of the member at a run order position.
	 */
	private @Nonnull String element(int position) {
		if (position < setUps.size()) {
			return "setUp";
		}
		return (position < setUps.size() + members.size()) ? "member" : "restore";
	}

	/**
	 * Describes the member at a run order position for a message, for example "&lt;setUp&gt; 00-backup.xml".
	 */
	private @Nonnull String describe(int position, @Nonnull SuiteMember member) {
		return "<" + element(position) + "> " + member.getFile();
	}

	/**
	 * Returns the index of a member in a list by identity, or -1.
	 */
	private static int indexOf(@Nonnull List<? extends SuiteMember> list, @Nonnull SuiteMember member) {
		for (int i = 0, l = list.size(); i < l; i++) {
			if (list.get(i) == member) {
				return i;
			}
		}
		return -1;
	}
}
