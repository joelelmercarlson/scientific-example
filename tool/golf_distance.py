import numpy as np
import sys

__author__  = "Joel E Carlson"
__credits__ = [ "joel.elmer.carlson@outlook.com" ]
__email__   = __credits__[0]

def simulate_golf_ball_flight(v0: float, launch_angle_deg: float, dt=0.01):
    """
    run
    """
    g   = 9.81  # gravity m/s^2
    rho = 1.225 # air density kg/m^3
    Cd  = 0.25  # drag coefficient
    Cl  = 0.2   # lift coefficient
    A   = 0.001432 # cross-section area m^2
    m   = 0.04593  # golf ball mass
    yd  = 1.09361  # yard
    mph = 2.23694  # mph

    initial_speed_mph = v0 * mph
    
    launch_angle = np.radians(launch_angle_deg)
    vx = v0 * np.cos(launch_angle)
    vy = v0 * np.sin(launch_angle)

    x, y = 0, 0
    xs = [x]
    ys = [y]
    max_height = 0.0
    time = 0.0
    prev_vx = vx
    prev_vy = vy
    
    # Time integration
    while y >= 0:
        v = np.sqrt(vx**2 + vy**2)
        if v == 0:
            break
        
        # ball-flight
        if y > max_height:
            max_height = y
        
        prev_vx = vx
        prev_vy = vy

        # Aerodynamic force
        Fd = 0.5 * rho * Cd * A * v**2
        Fl = 0.5 * rho * Cl * A * v**2

        # Drag components
        Fdx = -Fd * (vx/v)
        Fdy = -Fd * (vy/v)

        # Lift components
        FLx = Fl * (vy/v)
        FLy = -Fl * (vx/v)

        # Accelerations
        ax = (Fdx + FLx)/m
        ay = (Fdy + FLy)/m - g

        # Update velocities
        vx += ax * dt
        vy += ay * dt

        # positions
        x += vx * dt
        y += vy * dt

        # positions
        xs.append(x)
        ys.append(y)

        time += dt

    # landing angle
    landing_angle_rad = np.arctan2(prev_vy, prev_vx)
    landing_angle_deg = np.degrees(landing_angle_rad)

    # final ball speed at landing m/s
    landing_speed_mps = np.sqrt(prev_vx**2 + prev_vy**2)
    landing_speed_mph = landing_speed_mps * mph

    # Convert meters to yards (1 meter = 1.09361 yards)
    carry_yards = x * yd
    total_yards = carry_yards * 1.10
    max_height_yards = max_height * yd

    return {
        "carry_yards": carry_yards,
        "total_yards": total_yards,
        "max_height_yards": max_height_yards,
        "time_of_flight_seconds": time,
        "landing_angle_degrees": landing_angle_deg,
        "landing_speed_mph": landing_speed_mph,
        "initial_speed_mph": initial_speed_mph,
        "launch_angle_degrees": launch_angle_deg,
        "initial_speed_mps": v0,
        "x_values": xs,
        "y_values": ys,
        }

def display_coords(xs: [float], ys: [float]):
    """
    coords
    """
    coords = zip(xs, ys)
    for x, y in coords:
        print(f"""({x:2f}, {y:2f})""")

if __name__ == "__main__":
    # uv run golf_distance 70 12
    v0  = 70
    deg = 12.5
    if len(sys.argv) == 3:
        v0 = float(sys.argv[1])
        deg = float(sys.argv[2])
        result = simulate_golf_ball_flight(v0, deg)
    else:
        print ("""provide velocity degree. example 90, 12.5""")
        result = simulate_golf_ball_flight (v0, deg)
    carry = result.get("carry_yards", 0.0)
    total = result.get("total_yards", 0.0)
    x_values = result.get("x_values", [])
    y_values = result.get("y_values",[])
    display_coords(x_values, y_values)
    print(f"""
simulated ball-flight distance: carry {carry:2f} and total {total:2f} yards""")
