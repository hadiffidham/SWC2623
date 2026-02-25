#!/usr/bin/env python3
"""
Simple HTTP server to serve the log viewer
"""

import http.server
import socketserver
import webbrowser
from threading import Timer

PORT = 8000
Handler = http.server.SimpleHTTPRequestHandler

def open_browser():
    """Open browser after server starts"""
    webbrowser.open_new(f'http://localhost:{PORT}/view_log.html')

print("="*60)
print("📋 UNIVERSITY MANAGEMENT SYSTEM - LOG VIEWER SERVER")
print("="*60)
print(f"\nStarting server at http://localhost:{PORT}")
print("Press Ctrl+C to stop")
print("\nAvailable files:")
print(f"  • view_log.html - Log viewer interface")
print(f"  • analysis_log.txt - Generated log file (run run_analysis.py first)")
print("="*60)

# Open browser after 1 second
Timer(1, open_browser).start()

# Start server
with socketserver.TCPServer(("", PORT), Handler) as httpd:
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n\nServer stopped.")