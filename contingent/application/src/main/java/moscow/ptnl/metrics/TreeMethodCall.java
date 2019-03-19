package moscow.ptnl.metrics;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/** Узел дерева логов, который умеет считать время своего выполнения
 * Created by abelyakov on 12.08.15.
 */
class TreeMethodCall {
    private static final String ROOT = "ROOT";
    final TreeMethodCall parent;
    private final String name;
    private final long startTime;
    private long endTime;
    private List<TreeMethodCall> children = null;

    private TreeMethodCall(String name, TreeMethodCall parent) {
        this.name = name;
        this.startTime = System.currentTimeMillis();
        this.parent = parent;
    }

    static TreeMethodCall root() {
        return new TreeMethodCall(ROOT, null);
    }

    public static TreeMethodCall create(String name, TreeMethodCall parent) {
        return new TreeMethodCall(name, parent);
    }

    void addNestedCall(TreeMethodCall call) {
        if (children == null) {
            children = new LinkedList<>();
        }
        children.add(call);
    }

    void stopWatch() {
        endTime = System.currentTimeMillis();
    }

    private long millisElapsed() {
        if (endTime == 0)
            throw new RuntimeException("time measurement is not ended yet!");
        return endTime - startTime;
    }

    private long cleanTime() {
        long millisElapsed = millisElapsed();
        if (children != null) {
            long dirties = children.stream().mapToLong(TreeMethodCall::millisElapsed).sum();
            return millisElapsed - dirties;
        } else {
            return millisElapsed;
        }
    }

    @Override
    public String toString() {
        return String.format(
                "%-50s%-20s%s%n",
                name,
                " dirtyTime: " + millisElapsed() + "ms",
                " clean time: " + cleanTime() + "ms");
    }

    /***
     * Конвертировать в древовидное представление, которое умеет себя распечатать
     *
     */
    LogNode toLogNode() {
        List<LogNode> lnChilds = children == null
                ? null
                : children.stream()
                .map(TreeMethodCall::toLogNode)
                .collect(Collectors.toList());
        return new LogNode(getLogMessage(), lnChilds);
    }

    private String getLogMessage() {
        return new StringBuilder().append(name)
                .append(Objects.equals(name, ROOT) ? "" : "()")
                .append(" ")
                .append(millisElapsed())
                .append("ms")
                .toString();
    }
}