//! Build configuration for Klar MCP Server.
//!
//! ## Build Commands
//! - `zig build` - Build the mcp-klar executable
//! - `zig build run` - Build and run the MCP server
//! - `zig build test` - Run all tests

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build options
    const enable_logging = b.option(
        bool,
        "log",
        "Enable debug logging",
    ) orelse (optimize == .Debug);

    const options = b.addOptions();
    options.addOption(bool, "enable_logging", enable_logging);
    const build_options_mod = options.createModule();

    // Runtime configuration module
    const config_mod = b.addModule("config", .{
        .root_source_file = b.path("src/config.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Utility module
    const utils_mod = b.addModule("utils", .{
        .root_source_file = b.path("src/utils/root.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Compiler module
    const compiler_mod = b.addModule("compiler", .{
        .root_source_file = b.path("src/compiler/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "utils", .module = utils_mod },
        },
    });

    // Analysis module
    const analysis_mod = b.addModule("analysis", .{
        .root_source_file = b.path("src/analysis/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "utils", .module = utils_mod },
            .{ .name = "compiler", .module = compiler_mod },
        },
    });

    // MCP module
    const mcp_mod = b.addModule("mcp", .{
        .root_source_file = b.path("src/mcp/root.zig"),
        .target = target,
        .optimize = optimize,
        .imports = &.{
            .{ .name = "utils", .module = utils_mod },
            .{ .name = "compiler", .module = compiler_mod },
            .{ .name = "analysis", .module = analysis_mod },
            .{ .name = "config", .module = config_mod },
            .{ .name = "build_options", .module = build_options_mod },
        },
    });

    // Main executable
    const exe = b.addExecutable(.{
        .name = "mcp-klar",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "utils", .module = utils_mod },
                .{ .name = "compiler", .module = compiler_mod },
                .{ .name = "analysis", .module = analysis_mod },
                .{ .name = "mcp", .module = mcp_mod },
                .{ .name = "config", .module = config_mod },
                .{ .name = "build_options", .module = build_options_mod },
            },
        }),
    });
    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the MCP server");
    run_step.dependOn(&run_cmd.step);

    // Unit tests for each module
    const utils_tests = b.addTest(.{
        .root_module = utils_mod,
    });
    const compiler_tests = b.addTest(.{
        .root_module = compiler_mod,
    });
    const analysis_tests = b.addTest(.{
        .root_module = analysis_mod,
    });
    const mcp_tests = b.addTest(.{
        .root_module = mcp_mod,
    });
    const config_tests = b.addTest(.{
        .root_module = config_mod,
    });

    // Integration tests
    const integration_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/integration_test.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "mcp", .module = mcp_mod },
                .{ .name = "config", .module = config_mod },
                .{ .name = "utils", .module = utils_mod },
                .{ .name = "compiler", .module = compiler_mod },
                .{ .name = "analysis", .module = analysis_mod },
            },
        }),
    });

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&b.addRunArtifact(utils_tests).step);
    test_step.dependOn(&b.addRunArtifact(compiler_tests).step);
    test_step.dependOn(&b.addRunArtifact(analysis_tests).step);
    test_step.dependOn(&b.addRunArtifact(mcp_tests).step);
    test_step.dependOn(&b.addRunArtifact(config_tests).step);
    test_step.dependOn(&b.addRunArtifact(integration_tests).step);

    // Integration test step (separate)
    const integration_test_step = b.step("test-integration", "Run integration tests only");
    integration_test_step.dependOn(&b.addRunArtifact(integration_tests).step);
}
